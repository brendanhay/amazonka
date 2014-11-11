{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.SecurityToken.AssumeRoleWithSAML
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a set of temporary security credentials for users who have been
-- authenticated via a SAML authentication response. This operation provides a
-- mechanism for tying an enterprise identity store or directory to role-based
-- AWS access without user-specific credentials or configuration. The
-- temporary security credentials returned by this operation consist of an
-- access key ID, a secret access key, and a security token. Applications can
-- use these temporary security credentials to sign calls to AWS services. The
-- credentials are valid for the duration that you specified when calling
-- AssumeRoleWithSAML, which can be up to 3600 seconds (1 hour) or until the
-- time specified in the SAML authentication response's NotOnOrAfter value,
-- whichever is shorter. Optionally, you can pass an IAM access policy to this
-- operation. If you choose not to pass a policy, the temporary security
-- credentials that are returned by the operation have the permissions that
-- are defined in the access policy of the role that is being assumed. If you
-- pass a policy to this operation, the temporary security credentials that
-- are returned by the operation have the permissions that are allowed by both
-- the access policy of the role that is being assumed, and the policy that
-- you pass. This gives you a way to further restrict the permissions for the
-- resulting temporary security credentials. You cannot use the passed policy
-- to grant permissions that are in excess of those allowed by the access
-- policy of the role that is being assumed. For more information, see
-- Permissions for AssumeRoleWithSAML in Using Temporary Security Credentials.
-- Before your application can call AssumeRoleWithSAML, you must configure
-- your SAML identity provider (IdP) to issue the claims required by AWS.
-- Additionally, you must use AWS Identity and Access Management (IAM) to
-- create a SAML provider entity in your AWS account that represents your
-- identity provider, and create an IAM role that specifies this SAML provider
-- in its trust policy. Calling AssumeRoleWithSAML does not require the use of
-- AWS security credentials. The identity of the caller is validated by using
-- keys in the metadata document that is uploaded for the SAML provider entity
-- for your identity provider. For more information, see the following
-- resources: Creating Temporary Security Credentials for SAML Federation in
-- Using Temporary Security Credentials. SAML Providers in Using IAM.
-- Configuring a Relying Party and Claims in Using IAM. Creating a Role for
-- SAML-Based Federation in Using IAM.
module Network.AWS.SecurityToken.AssumeRoleWithSAML
    (
    -- * Request
      AssumeRoleWithSAML
    -- ** Request constructor
    , assumeRoleWithSAML
    -- ** Request lenses
    , arwsamlDurationSeconds
    , arwsamlPolicy
    , arwsamlPrincipalArn
    , arwsamlRoleArn
    , arwsamlSAMLAssertion

    -- * Response
    , AssumeRoleWithSAMLResponse
    -- ** Response constructor
    , assumeRoleWithSAMLResponse
    -- ** Response lenses
    , arwsamlrAssumedRoleUser
    , arwsamlrAudience
    , arwsamlrCredentials
    , arwsamlrIssuer
    , arwsamlrNameQualifier
    , arwsamlrPackedPolicySize
    , arwsamlrSubject
    , arwsamlrSubjectType
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SecurityToken.Types

data AssumeRoleWithSAML = AssumeRoleWithSAML
    { _arwsamlDurationSeconds :: Maybe Int
    , _arwsamlPolicy          :: Maybe Text
    , _arwsamlPrincipalArn    :: Text
    , _arwsamlRoleArn         :: Text
    , _arwsamlSAMLAssertion   :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'AssumeRoleWithSAML' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'arwsamlDurationSeconds' @::@ 'Maybe' 'Int'
--
-- * 'arwsamlPolicy' @::@ 'Maybe' 'Text'
--
-- * 'arwsamlPrincipalArn' @::@ 'Text'
--
-- * 'arwsamlRoleArn' @::@ 'Text'
--
-- * 'arwsamlSAMLAssertion' @::@ 'Text'
--
assumeRoleWithSAML :: Text -- ^ 'arwsamlRoleArn'
                   -> Text -- ^ 'arwsamlPrincipalArn'
                   -> Text -- ^ 'arwsamlSAMLAssertion'
                   -> AssumeRoleWithSAML
assumeRoleWithSAML p1 p2 p3 = AssumeRoleWithSAML
    { _arwsamlRoleArn         = p1
    , _arwsamlPrincipalArn    = p2
    , _arwsamlSAMLAssertion   = p3
    , _arwsamlPolicy          = Nothing
    , _arwsamlDurationSeconds = Nothing
    }

-- | The duration, in seconds, of the role session. The value can range from
-- 900 seconds (15 minutes) to 3600 seconds (1 hour). By default, the value
-- is set to 3600 seconds. An expiration can also be specified in the SAML
-- authentication response's NotOnOrAfter value. The actual expiration time
-- is whichever value is shorter.
arwsamlDurationSeconds :: Lens' AssumeRoleWithSAML (Maybe Int)
arwsamlDurationSeconds =
    lens _arwsamlDurationSeconds (\s a -> s { _arwsamlDurationSeconds = a })

-- | An IAM policy in JSON format. The policy parameter is optional. If you
-- pass a policy, the temporary security credentials that are returned by
-- the operation have the permissions that are allowed by both the access
-- policy of the role that is being assumed, and the policy that you pass.
-- This gives you a way to further restrict the permissions for the
-- resulting temporary security credentials. You cannot use the passed
-- policy to grant permissions that are in excess of those allowed by the
-- access policy of the role that is being assumed. For more information,
-- see Permissions for AssumeRoleWithSAML in Using Temporary Security
-- Credentials.
arwsamlPolicy :: Lens' AssumeRoleWithSAML (Maybe Text)
arwsamlPolicy = lens _arwsamlPolicy (\s a -> s { _arwsamlPolicy = a })

-- | The Amazon Resource Name (ARN) of the SAML provider in IAM that describes
-- the IdP.
arwsamlPrincipalArn :: Lens' AssumeRoleWithSAML Text
arwsamlPrincipalArn =
    lens _arwsamlPrincipalArn (\s a -> s { _arwsamlPrincipalArn = a })

-- | The Amazon Resource Name (ARN) of the role that the caller is assuming.
arwsamlRoleArn :: Lens' AssumeRoleWithSAML Text
arwsamlRoleArn = lens _arwsamlRoleArn (\s a -> s { _arwsamlRoleArn = a })

-- | The base-64 encoded SAML authentication response provided by the IdP. For
-- more information, see Configuring a Relying Party and Adding Claims in
-- the Using IAM guide.
arwsamlSAMLAssertion :: Lens' AssumeRoleWithSAML Text
arwsamlSAMLAssertion =
    lens _arwsamlSAMLAssertion (\s a -> s { _arwsamlSAMLAssertion = a })
instance ToQuery AssumeRoleWithSAML

instance ToPath AssumeRoleWithSAML where
    toPath = const "/"

data AssumeRoleWithSAMLResponse = AssumeRoleWithSAMLResponse
    { _arwsamlrAssumedRoleUser  :: Maybe AssumedRoleUser
    , _arwsamlrAudience         :: Maybe Text
    , _arwsamlrCredentials      :: Maybe Credentials
    , _arwsamlrIssuer           :: Maybe Text
    , _arwsamlrNameQualifier    :: Maybe Text
    , _arwsamlrPackedPolicySize :: Maybe Int
    , _arwsamlrSubject          :: Maybe Text
    , _arwsamlrSubjectType      :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'AssumeRoleWithSAMLResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'arwsamlrAssumedRoleUser' @::@ 'Maybe' 'AssumedRoleUser'
--
-- * 'arwsamlrAudience' @::@ 'Maybe' 'Text'
--
-- * 'arwsamlrCredentials' @::@ 'Maybe' 'Credentials'
--
-- * 'arwsamlrIssuer' @::@ 'Maybe' 'Text'
--
-- * 'arwsamlrNameQualifier' @::@ 'Maybe' 'Text'
--
-- * 'arwsamlrPackedPolicySize' @::@ 'Maybe' 'Int'
--
-- * 'arwsamlrSubject' @::@ 'Maybe' 'Text'
--
-- * 'arwsamlrSubjectType' @::@ 'Maybe' 'Text'
--
assumeRoleWithSAMLResponse :: AssumeRoleWithSAMLResponse
assumeRoleWithSAMLResponse = AssumeRoleWithSAMLResponse
    { _arwsamlrCredentials      = Nothing
    , _arwsamlrAssumedRoleUser  = Nothing
    , _arwsamlrPackedPolicySize = Nothing
    , _arwsamlrSubject          = Nothing
    , _arwsamlrSubjectType      = Nothing
    , _arwsamlrIssuer           = Nothing
    , _arwsamlrAudience         = Nothing
    , _arwsamlrNameQualifier    = Nothing
    }

arwsamlrAssumedRoleUser :: Lens' AssumeRoleWithSAMLResponse (Maybe AssumedRoleUser)
arwsamlrAssumedRoleUser =
    lens _arwsamlrAssumedRoleUser (\s a -> s { _arwsamlrAssumedRoleUser = a })

-- | The value of the Recipient attribute of the SubjectConfirmationData
-- element of the SAML assertion.
arwsamlrAudience :: Lens' AssumeRoleWithSAMLResponse (Maybe Text)
arwsamlrAudience = lens _arwsamlrAudience (\s a -> s { _arwsamlrAudience = a })

arwsamlrCredentials :: Lens' AssumeRoleWithSAMLResponse (Maybe Credentials)
arwsamlrCredentials =
    lens _arwsamlrCredentials (\s a -> s { _arwsamlrCredentials = a })

-- | The value of the Issuer element of the SAML assertion.
arwsamlrIssuer :: Lens' AssumeRoleWithSAMLResponse (Maybe Text)
arwsamlrIssuer = lens _arwsamlrIssuer (\s a -> s { _arwsamlrIssuer = a })

-- | A hash value based on the concatenation of the Issuer response value, the
-- AWS account ID, and the friendly name (the last part of the ARN) of the
-- SAML provider in IAM. The combination of NameQualifier and Subject can be
-- used to uniquely identify a federated user. The following pseudocode
-- shows how the hash value is calculated: BASE64 ( SHA1 (
-- "https://example.com/saml" + "123456789012" + "/MySAMLIdP" ) ).
arwsamlrNameQualifier :: Lens' AssumeRoleWithSAMLResponse (Maybe Text)
arwsamlrNameQualifier =
    lens _arwsamlrNameQualifier (\s a -> s { _arwsamlrNameQualifier = a })

-- | A percentage value that indicates the size of the policy in packed form.
-- The service rejects any policy with a packed size greater than 100
-- percent, which means the policy exceeded the allowed space.
arwsamlrPackedPolicySize :: Lens' AssumeRoleWithSAMLResponse (Maybe Int)
arwsamlrPackedPolicySize =
    lens _arwsamlrPackedPolicySize
        (\s a -> s { _arwsamlrPackedPolicySize = a })

-- | The value of the NameID element in the Subject element of the SAML
-- assertion.
arwsamlrSubject :: Lens' AssumeRoleWithSAMLResponse (Maybe Text)
arwsamlrSubject = lens _arwsamlrSubject (\s a -> s { _arwsamlrSubject = a })

-- | The format of the name ID, as defined by the Format attribute in the
-- NameID element of the SAML assertion. Typical examples of the format are
-- transient or persistent. If the format includes the prefix
-- urn:oasis:names:tc:SAML:2.0:nameid-format, that prefix is removed. For
-- example, urn:oasis:names:tc:SAML:2.0:nameid-format:transient is returned
-- as transient. If the format includes any other prefix, the format is
-- returned with no modifications.
arwsamlrSubjectType :: Lens' AssumeRoleWithSAMLResponse (Maybe Text)
arwsamlrSubjectType =
    lens _arwsamlrSubjectType (\s a -> s { _arwsamlrSubjectType = a })
instance FromXML AssumeRoleWithSAMLResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AssumeRoleWithSAMLResponse"

instance AWSRequest AssumeRoleWithSAML where
    type Sv AssumeRoleWithSAML = SecurityToken
    type Rs AssumeRoleWithSAML = AssumeRoleWithSAMLResponse

    request  = post "AssumeRoleWithSAML"
    response = xmlResponse $ \h x -> AssumeRoleWithSAMLResponse
        <$> x %| "AssumedRoleUser"
        <*> x %| "Audience"
        <*> x %| "Credentials"
        <*> x %| "Issuer"
        <*> x %| "NameQualifier"
        <*> x %| "PackedPolicySize"
        <*> x %| "Subject"
        <*> x %| "SubjectType"
