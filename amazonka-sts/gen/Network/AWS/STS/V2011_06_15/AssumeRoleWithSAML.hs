{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.STS.V2011_06_15.AssumeRoleWithSAML
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
module Network.AWS.STS.V2011_06_15.AssumeRoleWithSAML
    (
    -- * Request
      AssumeRoleWithSAML
    -- ** Request constructor
    , assumeRoleWithSAML
    -- ** Request lenses
    , arwsamlrRoleArn
    , arwsamlrPrincipalArn
    , arwsamlrSAMLAssertion
    , arwsamlrDurationSeconds
    , arwsamlrPolicy

    -- * Response
    , AssumeRoleWithSAMLResponse
    -- ** Response lenses
    , arwsamlsAssumedRoleUser
    , arwsamlsAudience
    , arwsamlsCredentials
    , arwsamlsIssuer
    , arwsamlsNameQualifier
    , arwsamlsPackedPolicySize
    , arwsamlsSubject
    , arwsamlsSubjectType
    ) where

import Network.AWS.Request.Query
import Network.AWS.STS.V2011_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'AssumeRoleWithSAML' request.
assumeRoleWithSAML :: Text -- ^ 'arwsamlrRoleArn'
                   -> Text -- ^ 'arwsamlrPrincipalArn'
                   -> Text -- ^ 'arwsamlrSAMLAssertion'
                   -> AssumeRoleWithSAML
assumeRoleWithSAML p1 p2 p3 = AssumeRoleWithSAML
    { _arwsamlrRoleArn = p1
    , _arwsamlrPrincipalArn = p2
    , _arwsamlrSAMLAssertion = p3
    , _arwsamlrDurationSeconds = Nothing
    , _arwsamlrPolicy = Nothing
    }

data AssumeRoleWithSAML = AssumeRoleWithSAML
    { _arwsamlrRoleArn :: Text
      -- ^ The Amazon Resource Name (ARN) of the role that the caller is
      -- assuming.
    , _arwsamlrPrincipalArn :: Text
      -- ^ The Amazon Resource Name (ARN) of the SAML provider in IAM that
      -- describes the IdP.
    , _arwsamlrSAMLAssertion :: Text
      -- ^ The base-64 encoded SAML authentication response provided by the
      -- IdP. For more information, see Configuring a Relying Party and
      -- Adding Claims in the Using IAM guide.
    , _arwsamlrDurationSeconds :: Maybe Integer
      -- ^ The duration, in seconds, of the role session. The value can
      -- range from 900 seconds (15 minutes) to 3600 seconds (1 hour). By
      -- default, the value is set to 3600 seconds. An expiration can also
      -- be specified in the SAML authentication response's NotOnOrAfter
      -- value. The actual expiration time is whichever value is shorter.
      -- The maximum duration for a session is 1 hour, and the minimum
      -- duration is 15 minutes, even if values outside this range are
      -- specified.
    , _arwsamlrPolicy :: Maybe Text
      -- ^ An IAM policy in JSON format. The policy parameter is optional.
      -- If you pass a policy, the temporary security credentials that are
      -- returned by the operation have the permissions that are allowed
      -- by both the access policy of the role that is being assumed, and
      -- the policy that you pass. This gives you a way to further
      -- restrict the permissions for the resulting temporary security
      -- credentials. You cannot use the passed policy to grant
      -- permissions that are in excess of those allowed by the access
      -- policy of the role that is being assumed. For more information,
      -- see Permissions for AssumeRoleWithSAML in Using Temporary
      -- Security Credentials. The policy must be 2048 bytes or shorter,
      -- and its packed size must be less than 450 bytes.
    } deriving (Show, Generic)

-- | The Amazon Resource Name (ARN) of the role that the caller is assuming.
arwsamlrRoleArn
    :: Functor f
    => (Text
    -> f (Text))
    -> AssumeRoleWithSAML
    -> f AssumeRoleWithSAML
arwsamlrRoleArn f x =
    (\y -> x { _arwsamlrRoleArn = y })
       <$> f (_arwsamlrRoleArn x)
{-# INLINE arwsamlrRoleArn #-}

-- | The Amazon Resource Name (ARN) of the SAML provider in IAM that describes
-- the IdP.
arwsamlrPrincipalArn
    :: Functor f
    => (Text
    -> f (Text))
    -> AssumeRoleWithSAML
    -> f AssumeRoleWithSAML
arwsamlrPrincipalArn f x =
    (\y -> x { _arwsamlrPrincipalArn = y })
       <$> f (_arwsamlrPrincipalArn x)
{-# INLINE arwsamlrPrincipalArn #-}

-- | The base-64 encoded SAML authentication response provided by the IdP. For
-- more information, see Configuring a Relying Party and Adding Claims in the
-- Using IAM guide.
arwsamlrSAMLAssertion
    :: Functor f
    => (Text
    -> f (Text))
    -> AssumeRoleWithSAML
    -> f AssumeRoleWithSAML
arwsamlrSAMLAssertion f x =
    (\y -> x { _arwsamlrSAMLAssertion = y })
       <$> f (_arwsamlrSAMLAssertion x)
{-# INLINE arwsamlrSAMLAssertion #-}

-- | The duration, in seconds, of the role session. The value can range from 900
-- seconds (15 minutes) to 3600 seconds (1 hour). By default, the value is set
-- to 3600 seconds. An expiration can also be specified in the SAML
-- authentication response's NotOnOrAfter value. The actual expiration time is
-- whichever value is shorter. The maximum duration for a session is 1 hour,
-- and the minimum duration is 15 minutes, even if values outside this range
-- are specified.
arwsamlrDurationSeconds
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> AssumeRoleWithSAML
    -> f AssumeRoleWithSAML
arwsamlrDurationSeconds f x =
    (\y -> x { _arwsamlrDurationSeconds = y })
       <$> f (_arwsamlrDurationSeconds x)
{-# INLINE arwsamlrDurationSeconds #-}

-- | An IAM policy in JSON format. The policy parameter is optional. If you pass
-- a policy, the temporary security credentials that are returned by the
-- operation have the permissions that are allowed by both the access policy
-- of the role that is being assumed, and the policy that you pass. This gives
-- you a way to further restrict the permissions for the resulting temporary
-- security credentials. You cannot use the passed policy to grant permissions
-- that are in excess of those allowed by the access policy of the role that
-- is being assumed. For more information, see Permissions for
-- AssumeRoleWithSAML in Using Temporary Security Credentials. The policy must
-- be 2048 bytes or shorter, and its packed size must be less than 450 bytes.
arwsamlrPolicy
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AssumeRoleWithSAML
    -> f AssumeRoleWithSAML
arwsamlrPolicy f x =
    (\y -> x { _arwsamlrPolicy = y })
       <$> f (_arwsamlrPolicy x)
{-# INLINE arwsamlrPolicy #-}

instance ToQuery AssumeRoleWithSAML where
    toQuery = genericQuery def

data AssumeRoleWithSAMLResponse = AssumeRoleWithSAMLResponse
    { _arwsamlsAssumedRoleUser :: Maybe AssumedRoleUser
      -- ^ The identifiers for the temporary security credentials that the
      -- operation returns.
    , _arwsamlsAudience :: Maybe Text
    , _arwsamlsCredentials :: Maybe Credentials
      -- ^ AWS credentials for API authentication.
    , _arwsamlsIssuer :: Maybe Text
    , _arwsamlsNameQualifier :: Maybe Text
    , _arwsamlsPackedPolicySize :: Maybe Integer
      -- ^ A percentage value that indicates the size of the policy in
      -- packed form. The service rejects any policy with a packed size
      -- greater than 100 percent, which means the policy exceeded the
      -- allowed space.
    , _arwsamlsSubject :: Maybe Text
    , _arwsamlsSubjectType :: Maybe Text
    } deriving (Show, Generic)

-- | The identifiers for the temporary security credentials that the operation
-- returns.
arwsamlsAssumedRoleUser
    :: Functor f
    => (Maybe AssumedRoleUser
    -> f (Maybe AssumedRoleUser))
    -> AssumeRoleWithSAMLResponse
    -> f AssumeRoleWithSAMLResponse
arwsamlsAssumedRoleUser f x =
    (\y -> x { _arwsamlsAssumedRoleUser = y })
       <$> f (_arwsamlsAssumedRoleUser x)
{-# INLINE arwsamlsAssumedRoleUser #-}

arwsamlsAudience
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AssumeRoleWithSAMLResponse
    -> f AssumeRoleWithSAMLResponse
arwsamlsAudience f x =
    (\y -> x { _arwsamlsAudience = y })
       <$> f (_arwsamlsAudience x)
{-# INLINE arwsamlsAudience #-}

-- | AWS credentials for API authentication.
arwsamlsCredentials
    :: Functor f
    => (Maybe Credentials
    -> f (Maybe Credentials))
    -> AssumeRoleWithSAMLResponse
    -> f AssumeRoleWithSAMLResponse
arwsamlsCredentials f x =
    (\y -> x { _arwsamlsCredentials = y })
       <$> f (_arwsamlsCredentials x)
{-# INLINE arwsamlsCredentials #-}

arwsamlsIssuer
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AssumeRoleWithSAMLResponse
    -> f AssumeRoleWithSAMLResponse
arwsamlsIssuer f x =
    (\y -> x { _arwsamlsIssuer = y })
       <$> f (_arwsamlsIssuer x)
{-# INLINE arwsamlsIssuer #-}

arwsamlsNameQualifier
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AssumeRoleWithSAMLResponse
    -> f AssumeRoleWithSAMLResponse
arwsamlsNameQualifier f x =
    (\y -> x { _arwsamlsNameQualifier = y })
       <$> f (_arwsamlsNameQualifier x)
{-# INLINE arwsamlsNameQualifier #-}

-- | A percentage value that indicates the size of the policy in packed form.
-- The service rejects any policy with a packed size greater than 100 percent,
-- which means the policy exceeded the allowed space.
arwsamlsPackedPolicySize
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> AssumeRoleWithSAMLResponse
    -> f AssumeRoleWithSAMLResponse
arwsamlsPackedPolicySize f x =
    (\y -> x { _arwsamlsPackedPolicySize = y })
       <$> f (_arwsamlsPackedPolicySize x)
{-# INLINE arwsamlsPackedPolicySize #-}

arwsamlsSubject
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AssumeRoleWithSAMLResponse
    -> f AssumeRoleWithSAMLResponse
arwsamlsSubject f x =
    (\y -> x { _arwsamlsSubject = y })
       <$> f (_arwsamlsSubject x)
{-# INLINE arwsamlsSubject #-}

arwsamlsSubjectType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AssumeRoleWithSAMLResponse
    -> f AssumeRoleWithSAMLResponse
arwsamlsSubjectType f x =
    (\y -> x { _arwsamlsSubjectType = y })
       <$> f (_arwsamlsSubjectType x)
{-# INLINE arwsamlsSubjectType #-}

instance FromXML AssumeRoleWithSAMLResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest AssumeRoleWithSAML where
    type Sv AssumeRoleWithSAML = STS
    type Rs AssumeRoleWithSAML = AssumeRoleWithSAMLResponse

    request = post "AssumeRoleWithSAML"
    response _ = xmlResponse
