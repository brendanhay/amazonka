{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.STS.V2011_06_15.Monadic
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The AWS Security Token Service (STS) is a web service that enables you to
-- request temporary, limited-privilege credentials for AWS Identity and
-- Access Management (IAM) users or for users that you authenticate (federated
-- users).
--
-- This module is provided for convenience. It offers an alternative to the
-- common idiom of supplying required fields to an operations's smart constructor,
-- using the operation's lenses to modify additional fields, and then sending
-- or paginating the request.
--
-- As an example: using "Network.AWS.STS" with the smart constructor and
-- basic lens syntax, before explicitly calling 'send':
--
-- @
-- import Control.Monad.Trans.AWS
-- import Network.AWS.STS
--
-- send $ (mkOperationName w x)
--      & onLensField1 .~ y
--      & onLensField2 .~ z
-- @
--
-- Versus using "Network.AWS.STS.Monadic" with the 'State' operator variants from
-- "Control.Lens.Setter" such as '.=' to modify any additional request
-- parameters before sending:
--
-- @
-- import Network.AWS.STS.Monadic
--
-- operationName w x $ do
--     onLensField1 .= y
--     onLensField2 .= z
-- @
--
module Network.AWS.STS.V2011_06_15.Monadic
    (
    -- * AssumeRole
    -- $AssumeRole
      assumeRole
    , assumeRoleCatch

    -- * AssumeRoleWithSAML
    -- $AssumeRoleWithSAML
    , assumeRoleWithSAML
    , assumeRoleWithSAMLCatch

    -- * AssumeRoleWithWebIdentity
    -- $AssumeRoleWithWebIdentity
    , assumeRoleWithWebIdentity
    , assumeRoleWithWebIdentityCatch

    -- * DecodeAuthorizationMessage
    -- $DecodeAuthorizationMessage
    , decodeAuthorizationMessage
    , decodeAuthorizationMessageCatch

    -- * GetFederationToken
    -- $GetFederationToken
    , getFederationToken
    , getFederationTokenCatch

    -- * GetSessionToken
    -- $GetSessionToken
    , getSessionToken
    , getSessionTokenCatch

    -- * Re-exported
    , module Network.AWS.STS.V2011_06_15

    , (.=)
    , (?=)
    , (<>=)
    , (%=)
    ) where

import Control.Monad.Trans.AWS as AWS
import Network.AWS.Prelude
import Network.AWS.STS.V2011_06_15

type ServiceEr = Er STS


-- $AssumeRole
-- Returns a set of temporary security credentials (consisting of an access
-- key ID, a secret access key, and a security token) that you can use to
-- access AWS resources that you might not normally have access to. Typically,
-- you use AssumeRole for cross-account access or federation. Important: You
-- cannot call AssumeRole by using AWS account credentials; access will be
-- denied. You must use IAM user credentials or temporary security credentials
-- to call AssumeRole. For cross-account access, imagine that you own multiple
-- accounts and need to access resources in each account. You could create
-- long-term credentials in each account to access those resources. However,
-- managing all those credentials and remembering which one can access which
-- account can be time consuming. Instead, you can create one set of long-term
-- credentials in one account and then use temporary security credentials to
-- access all the other accounts by assuming roles in those accounts. For more
-- information about roles, see Roles in Using IAM. For federation, you can,
-- for example, grant single sign-on access to the AWS Management Console. If
-- you already have an identity and authentication system in your corporate
-- network, you don't have to recreate user identities in AWS in order to
-- grant those user identities access to AWS. Instead, after a user has been
-- authenticated, you call AssumeRole (and specify the role with the
-- appropriate permissions) to get temporary security credentials for that
-- user. With those temporary security credentials, you construct a sign-in
-- URL that users can use to access the console. For more information, see
-- Scenarios for Granting Temporary Access in Using Temporary Security
-- Credentials. The temporary security credentials are valid for the duration
-- that you specified when calling AssumeRole, which can be from 900 seconds
-- (15 minutes) to 3600 seconds (1 hour). The default is 1 hour. Optionally,
-- you can pass an IAM access policy to this operation. If you choose not to
-- pass a policy, the temporary security credentials that are returned by the
-- operation have the permissions that are defined in the access policy of the
-- role that is being assumed. If you pass a policy to this operation, the
-- temporary security credentials that are returned by the operation have the
-- permissions that are allowed by both the access policy of the role that is
-- being assumed, and the policy that you pass. This gives you a way to
-- further restrict the permissions for the resulting temporary security
-- credentials. You cannot use the passed policy to grant permissions that are
-- in excess of those allowed by the access policy of the role that is being
-- assumed. For more information, see Permissions for AssumeRole in Using
-- Temporary Security Credentials. To assume a role, your AWS account must be
-- trusted by the role. The trust relationship is defined in the role's trust
-- policy when the role is created. You must also have a policy that allows
-- you to call sts:AssumeRole. Using MFA with AssumeRole You can optionally
-- include multi-factor authentication (MFA) information when you call
-- AssumeRole. This is useful for cross-account scenarios in which you want to
-- make sure that the user who is assuming the role has been authenticated
-- using an AWS MFA device. In that scenario, the trust policy of the role
-- being assumed includes a condition that tests for MFA authentication; if
-- the caller does not include valid MFA information, the request to assume
-- the role is denied. The condition in a trust policy that tests for MFA
-- authentication might look like the following example. "Condition": {"Null":
-- {"aws:MultiFactorAuthAge": false}} For more information, see Configuring
-- MFA-Protected API Access in the Using IAM guide. To use MFA with
-- AssumeRole, you pass values for the SerialNumber and TokenCode parameters.
-- The SerialNumber value identifies the user's hardware or virtual MFA
-- device. The TokenCode is the time-based one-time password (TOTP) that the
-- MFA devices produces. https://sts.amazonaws.com/ ?Version=2011-06-15
-- &Action=AssumeRole &RoleSessionName=Bob
-- &RoleArn=arn:aws:iam::123456789012:role/demo
-- &Policy=%7B%22Version%22%3A%222012-10-17%22%2C%22Statement%22%3A%5B%7B%22Sid%22%3A%22Stmt1%22%2C%22Effect%22%
-- 3A%22Allow%22%2C%22Action%22%3A%22s3%3A*%22%2C%22Resource%22%3A%22*%22%7D
-- %5D%7D &DurationSeconds=3600 &ExternalId=123ABC &AUTHPARAMS 2011-06-15/">
-- AQoDYXdzEPT//////////wEXAMPLEtc764bNrC9SAPBSM22wDOk4x4HIZ8j4FZTwdQW
-- LWsKWHGBuFqwAeMicRXmxfpSPfIeoIYRqTflfKD8YUuwthAx7mSEI/qkPpKPi/kMcGd
-- QrmGdeehM4IC1NtBmUpp2wUE8phUZampKsburEDy0KPkyQDYwT7WZ0wq5VSXDvp75YU
-- 9HFvlRd8Tx6q6fE8YQcHNVXAkiY9q6d+xo0rKwT38xVqr7ZD0u0iPPkUL64lIZbqBAz
-- +scqKmlzm8FDrypNC9Yjc8fPOLn9FX9KSYvKTr4rvx3iSIlTJabIQwj2ICCR/oLxBA==
-- wJalrXUtnFEMI/K7MDENG/bPxRfiCYzEXAMPLEKEY 2011-07-15T23:28:33.359Z
-- AKIAIOSFODNN7EXAMPLE arn:aws:sts::123456789012:assumed-role/demo/Bob
-- ARO123EXAMPLE123:Bob 6 c6104cbe-af31-11e0-8154-cbc7ccf896c7.
--
-- See: 'Network.AWS.STS.V2011_06_15.AssumeRole'

assumeRole :: ( MonadCatch m
              , MonadResource m
              , MonadError AWS.Error m
              , MonadReader Env m
              )
    => Text -- ^ 'arRoleArn'
    -> Text -- ^ 'arRoleSessionName'
    -> State AssumeRole a
    -> m AssumeRoleResponse
assumeRole p1 p2 s =
    send $ (mkAssumeRole p1 p2) &~ s

assumeRoleCatch :: ( MonadCatch m
                   , MonadResource m
                   , MonadReader Env m
                   )
    => Text -- ^ 'arRoleArn'
    -> Text -- ^ 'arRoleSessionName'
    -> State AssumeRole a
    -> m (Either ServiceEr AssumeRoleResponse)
assumeRoleCatch p1 p2 s =
    sendCatch $ (mkAssumeRole p1 p2) &~ s

-- $AssumeRoleWithSAML
-- Returns a set of temporary security credentials for users who have been
-- authenticated via a SAML authentication response. This operation provides a
-- mechanism for tying an enterprise identity store or directory to role-based
-- AWS access without user-specific credentials or configuration. The
-- temporary security credentials returned by this operation consist of an
-- access key ID, a secret access key, and a security token. Applications can
-- use these temporary security credentials to sign calls to AWS services. The
-- credentials are valid for the duration that you specified when calling
-- AssumeRoleWithSAML, which can be up to 3600 seconds (1 hour) or until the
-- time specified in the SAML authentication response's NotOnOrAfter value,
-- whichever is shorter. The maximum duration for a session is 1 hour, and the
-- minimum duration is 15 minutes, even if values outside this range are
-- specified. Optionally, you can pass an IAM access policy to this operation.
-- If you choose not to pass a policy, the temporary security credentials that
-- are returned by the operation have the permissions that are defined in the
-- access policy of the role that is being assumed. If you pass a policy to
-- this operation, the temporary security credentials that are returned by the
-- operation have the permissions that are allowed by both the access policy
-- of the role that is being assumed, and the policy that you pass. This gives
-- you a way to further restrict the permissions for the resulting temporary
-- security credentials. You cannot use the passed policy to grant permissions
-- that are in excess of those allowed by the access policy of the role that
-- is being assumed. For more information, see Permissions for
-- AssumeRoleWithSAML in Using Temporary Security Credentials. Before your
-- application can call AssumeRoleWithSAML, you must configure your SAML
-- identity provider (IdP) to issue the claims required by AWS. Additionally,
-- you must use AWS Identity and Access Management (IAM) to create a SAML
-- provider entity in your AWS account that represents your identity provider,
-- and create an IAM role that specifies this SAML provider in its trust
-- policy. Calling AssumeRoleWithSAML does not require the use of AWS security
-- credentials. The identity of the caller is validated by using keys in the
-- metadata document that is uploaded for the SAML provider entity for your
-- identity provider. For more information, see the following resources:
-- Creating Temporary Security Credentials for SAML Federation in Using
-- Temporary Security Credentials. SAML Providers in Using IAM. Configuring a
-- Relying Party and Claims in Using IAM. Creating a Role for SAML-Based
-- Federation in Using IAM.
--
-- See: 'Network.AWS.STS.V2011_06_15.AssumeRoleWithSAML'

assumeRoleWithSAML :: ( MonadCatch m
                      , MonadResource m
                      , MonadError AWS.Error m
                      , MonadReader Env m
                      )
    => Text -- ^ 'arwsamlRoleArn'
    -> Text -- ^ 'arwsamlPrincipalArn'
    -> Text -- ^ 'arwsamlSAMLAssertion'
    -> State AssumeRoleWithSAML a
    -> m AssumeRoleWithSAMLResponse
assumeRoleWithSAML p1 p2 p3 s =
    send $ (mkAssumeRoleWithSAML p1 p2 p3) &~ s

assumeRoleWithSAMLCatch :: ( MonadCatch m
                           , MonadResource m
                           , MonadReader Env m
                           )
    => Text -- ^ 'arwsamlRoleArn'
    -> Text -- ^ 'arwsamlPrincipalArn'
    -> Text -- ^ 'arwsamlSAMLAssertion'
    -> State AssumeRoleWithSAML a
    -> m (Either ServiceEr AssumeRoleWithSAMLResponse)
assumeRoleWithSAMLCatch p1 p2 p3 s =
    sendCatch $ (mkAssumeRoleWithSAML p1 p2 p3) &~ s

-- $AssumeRoleWithWebIdentity
-- Returns a set of temporary security credentials for users who have been
-- authenticated in a mobile or web application with a web identity provider,
-- such as Login with Amazon, Facebook, or Google. Calling
-- AssumeRoleWithWebIdentity does not require the use of AWS security
-- credentials. Therefore, you can distribute an application (for example, on
-- mobile devices) that requests temporary security credentials without
-- including long-term AWS credentials in the application, and without
-- deploying server-based proxy services that use long-term AWS credentials.
-- Instead, the identity of the caller is validated by using a token from the
-- web identity provider. The temporary security credentials returned by this
-- API consist of an access key ID, a secret access key, and a security token.
-- Applications can use these temporary security credentials to sign calls to
-- AWS service APIs. The credentials are valid for the duration that you
-- specified when calling AssumeRoleWithWebIdentity, which can be from 900
-- seconds (15 minutes) to 3600 seconds (1 hour). By default, the temporary
-- security credentials are valid for 1 hour. Optionally, you can pass an IAM
-- access policy to this operation. If you choose not to pass a policy, the
-- temporary security credentials that are returned by the operation have the
-- permissions that are defined in the access policy of the role that is being
-- assumed. If you pass a policy to this operation, the temporary security
-- credentials that are returned by the operation have the permissions that
-- are allowed by both the access policy of the role that is being assumed,
-- and the policy that you pass. This gives you a way to further restrict the
-- permissions for the resulting temporary security credentials. You cannot
-- use the passed policy to grant permissions that are in excess of those
-- allowed by the access policy of the role that is being assumed. For more
-- information, see Permissions for AssumeRoleWithWebIdentity in Using
-- Temporary Security Credentials. Before your application can call
-- AssumeRoleWithWebIdentity, you must have an identity token from a supported
-- identity provider and create a role that the application can assume. The
-- role that your application assumes must trust the identity provider that is
-- associated with the identity token. In other words, the identity provider
-- must be specified in the role's trust policy. For more information about
-- how to use web identity federation and the AssumeRoleWithWebIdentity, see
-- the following resources: Creating a Mobile Application with Third-Party
-- Sign-In and Creating Temporary Security Credentials for Mobile Apps Using
-- Third-Party Identity Providers in Using Temporary Security Credentials. Web
-- Identity Federation Playground. This interactive website lets you walk
-- through the process of authenticating via Login with Amazon, Facebook, or
-- Google, getting temporary security credentials, and then using those
-- credentials to make a request to AWS. AWS SDK for iOS and AWS SDK for
-- Android. These toolkits contain sample apps that show how to invoke the
-- identity providers, and then how to use the information from these
-- providers to get and use temporary security credentials. Web Identity
-- Federation with Mobile Applications. This article discusses web identity
-- federation and shows an example of how to use web identity federation to
-- get access to content in Amazon S3. https://sts.amazonaws.com/
-- ?Action=AssumeRoleWithWebIdentity &DurationSeconds=3600
-- &ProviderId=www.amazon.com &RoleSessionName=app1 &Version=2011-06-15
-- &RoleArn=arn%3Aaws%3Aiam%3A%3A000240903217%3Arole%2FFederatedWebIdentityRole
-- &WebIdentityToken=Atza%7CIQEBLjAsAhRFiXuWpUXuRvQ9PZL3GMFcYevydwIUFAHZwXZXX
-- XXXXXXJnrulxKDHwy87oGKPznh0D6bEQZTSCzyoCtL_8S07pLpr0zMbn6w1lfVZKNTBdDansFB
-- mtGnIsIapjI6xKR02Yc_2bQ8LZbUXSGm6Ry6_BG7PrtLZtj_dfCTj92xNGed-CrKqjG7nPBjNI
-- L016GGvuS5gSvPRUxWES3VYfm1wl7WTI7jn-Pcb6M-buCgHhFOzTQxod27L9CqnOLio7N3gZAG
-- psp6n1-AJBOCJckcyXe2c6uD0srOJeZlKUm2eTDVMf8IehDVI0r1QOnTV6KzzAI3OY87Vd_cVMQ
-- amzn1.account.AF6RHO7KZU5XRVQJGXK6HB56KR2A
-- arn:aws:sts::000240903217:assumed-role/FederatedWebIdentityRole/app1
-- AROACLKWSDQRAOFQC3IDI:app1 AQoDYXdzEE0a8ANXXXXXXXXNO1ewxE5TijQyp+IPfnyowF
-- wJalrXUtnFEMI/K7MDENG/bPxRfiCYzEXAMPLEKEY 2013-05-14T23:00:23Z
-- AKIAIOSFODNN7EXAMPLE apps.example.com
-- client.5498841531868486423.1548@apps.example.com
-- ad4156e9-bce1-11e2-82e6-6b6ef249e618.
--
-- See: 'Network.AWS.STS.V2011_06_15.AssumeRoleWithWebIdentity'

assumeRoleWithWebIdentity :: ( MonadCatch m
                             , MonadResource m
                             , MonadError AWS.Error m
                             , MonadReader Env m
                             )
    => Text -- ^ 'arwwiRoleArn'
    -> Text -- ^ 'arwwiRoleSessionName'
    -> Text -- ^ 'arwwiWebIdentityToken'
    -> State AssumeRoleWithWebIdentity a
    -> m AssumeRoleWithWebIdentityResponse
assumeRoleWithWebIdentity p1 p2 p3 s =
    send $ (mkAssumeRoleWithWebIdentity p1 p2 p3) &~ s

assumeRoleWithWebIdentityCatch :: ( MonadCatch m
                                  , MonadResource m
                                  , MonadReader Env m
                                  )
    => Text -- ^ 'arwwiRoleArn'
    -> Text -- ^ 'arwwiRoleSessionName'
    -> Text -- ^ 'arwwiWebIdentityToken'
    -> State AssumeRoleWithWebIdentity a
    -> m (Either ServiceEr AssumeRoleWithWebIdentityResponse)
assumeRoleWithWebIdentityCatch p1 p2 p3 s =
    sendCatch $ (mkAssumeRoleWithWebIdentity p1 p2 p3) &~ s

-- $DecodeAuthorizationMessage
-- Decodes additional information about the authorization status of a request
-- from an encoded message returned in response to an AWS request. For
-- example, if a user is not authorized to perform an action that he or she
-- has requested, the request returns a Client.UnauthorizedOperation response
-- (an HTTP 403 response). Some AWS actions additionally return an encoded
-- message that can provide details about this authorization failure. Only
-- certain AWS actions return an encoded authorization message. The
-- documentation for an individual action indicates whether that action
-- returns an encoded message in addition to returning an HTTP code. The
-- message is encoded because the details of the authorization status can
-- constitute privileged information that the user who requested the action
-- should not see. To decode an authorization status message, a user must be
-- granted permissions via an IAM policy to request the
-- DecodeAuthorizationMessage (sts:DecodeAuthorizationMessage) action. The
-- decoded message includes the following type of information: Whether the
-- request was denied due to an explicit deny or due to the absence of an
-- explicit allow. For more information, see Determining Whether a Request is
-- Allowed or Denied in Using IAM. The principal who made the request. The
-- requested action. The requested resource. The values of condition keys in
-- the context of the user's request. POST https://sts.amazonaws.com /
-- HTTP/1.1 Content-Type: application/x-www-form-urlencoded; charset=utf-8
-- Host: sts.amazonaws.com Content-Length: 1148 Expect: 100-continue
-- Connection: Keep-Alive Action=DecodeAuthorizationMessage &EncodedMessage=
-- &Version=2011-06-15 &AUTHPARAMS 6624a9ca-cd25-4f50-b2a5-7ba65bf07453 {
-- "allowed": "false", "explicitDeny": "false", "matchedStatements": "",
-- "failures": "", "context": { "principal": { "id": "AIDACKCEVSQ6C2EXAMPLE",
-- "name": "Bob", "arn": "arn:aws:iam::123456789012:user/Bob" }, "action":
-- "ec2:StopInstances", "resource":
-- "arn:aws:ec2:us-east-1:123456789012:instance/i-dd01c9bd", "conditions": [ {
-- "item": { "key": "ec2:Tenancy", "values": ["default"] }, { "item": { "key":
-- "ec2:ResourceTag/elasticbeanstalk:environment-name", "values":
-- ["Default-Environment"] } }, (Additional items ...) ] } }.
--
-- See: 'Network.AWS.STS.V2011_06_15.DecodeAuthorizationMessage'

decodeAuthorizationMessage :: ( MonadCatch m
                              , MonadResource m
                              , MonadError AWS.Error m
                              , MonadReader Env m
                              )
    => Text -- ^ 'damEncodedMessage'
    -> State DecodeAuthorizationMessage a
    -> m DecodeAuthorizationMessageResponse
decodeAuthorizationMessage p1 s =
    send $ (mkDecodeAuthorizationMessage p1) &~ s

decodeAuthorizationMessageCatch :: ( MonadCatch m
                                   , MonadResource m
                                   , MonadReader Env m
                                   )
    => Text -- ^ 'damEncodedMessage'
    -> State DecodeAuthorizationMessage a
    -> m (Either ServiceEr DecodeAuthorizationMessageResponse)
decodeAuthorizationMessageCatch p1 s =
    sendCatch $ (mkDecodeAuthorizationMessage p1) &~ s

-- $GetFederationToken
-- Returns a set of temporary security credentials (consisting of an access
-- key ID, a secret access key, and a security token) for a federated user. A
-- typical use is in a proxy application that gets temporary security
-- credentials on behalf of distributed applications inside a corporate
-- network. Because you must call the GetFederationToken action using the
-- long-term security credentials of an IAM user, this call is appropriate in
-- contexts where those credentials can be safely stored, usually in a
-- server-based application. Note: Do not use this call in mobile applications
-- or client-based web applications that directly get temporary security
-- credentials. For those types of applications, use
-- AssumeRoleWithWebIdentity. The GetFederationToken action must be called by
-- using the long-term AWS security credentials of an IAM user. You can also
-- call GetFederationToken using the security credentials of an AWS account
-- (root), but this is not recommended. Instead, we recommend that you create
-- an IAM user for the purpose of the proxy application and then attach a
-- policy to the IAM user that limits federated users to only the actions and
-- resources they need access to. For more information, see IAM Best Practices
-- in Using IAM. The temporary security credentials that are obtained by using
-- the long-term credentials of an IAM user are valid for the specified
-- duration, between 900 seconds (15 minutes) and 129600 seconds (36 hours).
-- Temporary credentials that are obtained by using AWS account (root)
-- credentials have a maximum duration of 3600 seconds (1 hour) Permissions
-- The permissions for the temporary security credentials returned by
-- GetFederationToken are determined by a combination of the following: The
-- policy or policies that are attached to the IAM user whose credentials are
-- used to call GetFederationToken. The policy that is passed as a parameter
-- in the call. The passed policy is attached to the temporary security
-- credentials that result from the GetFederationToken API call--that is, to
-- the federated user. When the federated user makes an AWS request, AWS
-- evaluates the policy attached to the federated user in combination with the
-- policy or policies attached to the IAM user whose credentials were used to
-- call GetFederationToken. AWS allows the federated user's request only when
-- both the federated user and the IAM user are explicitly allowed to perform
-- the requested action. The passed policy cannot grant more permissions than
-- those that are defined in the IAM user policy. A typical use case is that
-- the permissions of the IAM user whose credentials are used to call
-- GetFederationToken are designed to allow access to all the actions and
-- resources that any federated user will need. Then, for individual users,
-- you pass a policy to the operation that scopes down the permissions to a
-- level that's appropriate to that individual user, using a policy that
-- allows only a subset of permissions that are granted to the IAM user. If
-- you do not pass a policy, the resulting temporary security credentials have
-- no effective permissions. The only exception is when the temporary security
-- credentials are used to access a resource that has a resource-based policy
-- that specifically allows the federated user to access the resource. For
-- more information about how permissions work, see Permissions for
-- GetFederationToken in Using Temporary Security Credentials. For information
-- about using GetFederationToken to create temporary security credentials,
-- see Creating Temporary Credentials to Enable Access for Federated Users in
-- Using Temporary Security Credentials. https://sts.amazonaws.com/
-- ?Version=2011-06-15 &Action=GetFederationToken &Name=Bob
-- &Policy=%7B%22Version%22%3A%222012-10-17%22%2C%22Statement%22%3A%5B%7B%22Sid%22%3A%22Stmt1%22%2C%22Effect%22%
-- 3A%22Allow%22%2C%22Action%22%3A%22s3%3A*%22%2C%22Resource%22%3A%22*%22%7D
-- %5D%7D &DurationSeconds=3600 &AUTHPARAMS 2011-06-15/">
-- AQoDYXdzEPT//////////wEXAMPLEtc764bNrC9SAPBSM22wDOk4x4HIZ8j4FZTwdQW
-- LWsKWHGBuFqwAeMicRXmxfpSPfIeoIYRqTflfKD8YUuwthAx7mSEI/qkPpKPi/kMcGd
-- QrmGdeehM4IC1NtBmUpp2wUE8phUZampKsburEDy0KPkyQDYwT7WZ0wq5VSXDvp75YU
-- 9HFvlRd8Tx6q6fE8YQcHNVXAkiY9q6d+xo0rKwT38xVqr7ZD0u0iPPkUL64lIZbqBAz
-- +scqKmlzm8FDrypNC9Yjc8fPOLn9FX9KSYvKTr4rvx3iSIlTJabIQwj2ICCR/oLxBA==
-- wJalrXUtnFEMI/K7MDENG/bPxRfiCYzEXAMPLEKEY 2011-07-15T23:28:33.359Z
-- AKIAIOSFODNN7EXAMPLE arn:aws:sts::123456789012:federated-user/Bob
-- 123456789012:Bob 6 c6104cbe-af31-11e0-8154-cbc7ccf896c7.
--
-- See: 'Network.AWS.STS.V2011_06_15.GetFederationToken'

getFederationToken :: ( MonadCatch m
                      , MonadResource m
                      , MonadError AWS.Error m
                      , MonadReader Env m
                      )
    => Text -- ^ 'gftName'
    -> State GetFederationToken a
    -> m GetFederationTokenResponse
getFederationToken p1 s =
    send $ (mkGetFederationToken p1) &~ s

getFederationTokenCatch :: ( MonadCatch m
                           , MonadResource m
                           , MonadReader Env m
                           )
    => Text -- ^ 'gftName'
    -> State GetFederationToken a
    -> m (Either ServiceEr GetFederationTokenResponse)
getFederationTokenCatch p1 s =
    sendCatch $ (mkGetFederationToken p1) &~ s

-- $GetSessionToken
-- Returns a set of temporary credentials for an AWS account or IAM user. The
-- credentials consist of an access key ID, a secret access key, and a
-- security token. Typically, you use GetSessionToken if you want to use MFA
-- to protect programmatic calls to specific AWS APIs like Amazon EC2
-- StopInstances. MFA-enabled IAM users would need to call GetSessionToken and
-- submit an MFA code that is associated with their MFA device. Using the
-- temporary security credentials that are returned from the call, IAM users
-- can then make programmatic calls to APIs that require MFA authentication.
-- The GetSessionToken action must be called by using the long-term AWS
-- security credentials of the AWS account or an IAM user. Credentials that
-- are created by IAM users are valid for the duration that you specify,
-- between 900 seconds (15 minutes) and 129600 seconds (36 hours); credentials
-- that are created by using account credentials have a maximum duration of
-- 3600 seconds (1 hour). We recommend that you do not call GetSessionToken
-- with root account credentials. Instead, follow our best practices by
-- creating one or more IAM users, giving them the necessary permissions, and
-- using IAM users for everyday interaction with AWS. The permissions
-- associated with the temporary security credentials returned by
-- GetSessionToken are based on the permissions associated with account or IAM
-- user whose credentials are used to call the action. If GetSessionToken is
-- called using root account credentials, the temporary credentials have root
-- account permissions. Similarly, if GetSessionToken is called using the
-- credentials of an IAM user, the temporary credentials have the same
-- permissions as the IAM user. For more information about using
-- GetSessionToken to create temporary credentials, go to Creating Temporary
-- Credentials to Enable Access for IAM Users in Using Temporary Security
-- Credentials. https://sts.amazonaws.com/ ?Version=2011-06-15
-- &Action=GetSessionToken &DurationSeconds=3600
-- &SerialNumber=YourMFADeviceSerialNumber &TokenCode=123456 &AUTHPARAMS
-- AQoEXAMPLEH4aoAH0gNCAPyJxz4BlCFFxWNE1OPTgk5TthT+FvwqnKwRcOIfrRh3c/L
-- To6UDdyJwOOvEVPvLXCrrrUtdnniCEXAMPLE/IvU1dYUg2RVAJBanLiHb4IgRmpRV3z
-- rkuWJOgQs8IZZaIv2BXIa2R4OlgkBN9bkUDNCJiBeb/AXlzBBko7b15fjrBs2+cTQtp
-- Z3CYWFXG8C5zqx37wnOE49mRl/+OtkIKGO7fAE
-- wJalrXUtnFEMI/K7MDENG/bPxRfiCYzEXAMPLEKEY 2011-07-11T19:55:29.611Z
-- AKIAIOSFODNN7EXAMPLE 58c5dbae-abef-11e0-8cfe-09039844ac7d.
--
-- See: 'Network.AWS.STS.V2011_06_15.GetSessionToken'

getSessionToken :: ( MonadCatch m
                   , MonadResource m
                   , MonadError AWS.Error m
                   , MonadReader Env m
                   )
    => State GetSessionToken a
    -> m GetSessionTokenResponse
getSessionToken s =
    send (mkGetSessionToken &~ s)

getSessionTokenCatch :: ( MonadCatch m
                        , MonadResource m
                        , MonadReader Env m
                        )
    => State GetSessionToken a
    -> m (Either ServiceEr GetSessionTokenResponse)
getSessionTokenCatch s =
    sendCatch (mkGetSessionToken &~ s)
