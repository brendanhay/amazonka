{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.STS.AssumeRoleWithSAML
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns a set of temporary security credentials for users who have been
-- authenticated via a SAML authentication response. This operation
-- provides a mechanism for tying an enterprise identity store or directory
-- to role-based AWS access without user-specific credentials or
-- configuration.
--
-- The temporary security credentials returned by this operation consist of
-- an access key ID, a secret access key, and a security token.
-- Applications can use these temporary security credentials to sign calls
-- to AWS services. The credentials are valid for the duration that you
-- specified when calling @AssumeRoleWithSAML@, which can be up to 3600
-- seconds (1 hour) or until the time specified in the SAML authentication
-- response\'s @SessionNotOnOrAfter@ value, whichever is shorter.
--
-- The maximum duration for a session is 1 hour, and the minimum duration
-- is 15 minutes, even if values outside this range are specified.
--
-- Optionally, you can pass an IAM access policy to this operation. If you
-- choose not to pass a policy, the temporary security credentials that are
-- returned by the operation have the permissions that are defined in the
-- access policy of the role that is being assumed. If you pass a policy to
-- this operation, the temporary security credentials that are returned by
-- the operation have the permissions that are allowed by both the access
-- policy of the role that is being assumed, /__and__/ the policy that you
-- pass. This gives you a way to further restrict the permissions for the
-- resulting temporary security credentials. You cannot use the passed
-- policy to grant permissions that are in excess of those allowed by the
-- access policy of the role that is being assumed. For more information,
-- see
-- <http://docs.aws.amazon.com/STS/latest/UsingSTS/permissions-assume-role.html Permissions for AssumeRoleWithSAML>
-- in /Using Temporary Security Credentials/.
--
-- Before your application can call @AssumeRoleWithSAML@, you must
-- configure your SAML identity provider (IdP) to issue the claims required
-- by AWS. Additionally, you must use AWS Identity and Access Management
-- (IAM) to create a SAML provider entity in your AWS account that
-- represents your identity provider, and create an IAM role that specifies
-- this SAML provider in its trust policy.
--
-- Calling @AssumeRoleWithSAML@ does not require the use of AWS security
-- credentials. The identity of the caller is validated by using keys in
-- the metadata document that is uploaded for the SAML provider entity for
-- your identity provider.
--
-- For more information, see the following resources:
--
-- -   <http://docs.aws.amazon.com/STS/latest/UsingSTS/CreatingSAML.html Creating Temporary Security Credentials for SAML Federation>.
-- -   <http://docs.aws.amazon.com/IAM/latest/UserGuide/idp-managing-identityproviders.html SAML Providers>
--     in /Using IAM/.
-- -   <http://docs.aws.amazon.com/IAM/latest/UserGuide/create-role-saml-IdP-tasks.html Configuring a Relying Party and Claims>
--     in /Using IAM/.
-- -   <http://docs.aws.amazon.com/IAM/latest/UserGuide/create-role-saml.html Creating a Role for SAML-Based Federation>
--     in /Using IAM/.
--
-- <http://docs.aws.amazon.com/STS/latest/APIReference/API_AssumeRoleWithSAML.html>
module Network.AWS.STS.AssumeRoleWithSAML
    (
    -- * Request
      AssumeRoleWithSAML
    -- ** Request constructor
    , assumeRoleWithSAML
    -- ** Request lenses
    , arwsamlDurationSeconds
    , arwsamlPolicy
    , arwsamlRoleARN
    , arwsamlPrincipalARN
    , arwsamlSAMLAssertion

    -- * Response
    , AssumeRoleWithSAMLResponse
    -- ** Response constructor
    , assumeRoleWithSAMLResponse
    -- ** Response lenses
    , arwsamlrsAudience
    , arwsamlrsSubject
    , arwsamlrsPackedPolicySize
    , arwsamlrsCredentials
    , arwsamlrsSubjectType
    , arwsamlrsNameQualifier
    , arwsamlrsAssumedRoleUser
    , arwsamlrsIssuer
    , arwsamlrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.STS.Types

-- | /See:/ 'assumeRoleWithSAML' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'arwsamlDurationSeconds'
--
-- * 'arwsamlPolicy'
--
-- * 'arwsamlRoleARN'
--
-- * 'arwsamlPrincipalARN'
--
-- * 'arwsamlSAMLAssertion'
data AssumeRoleWithSAML = AssumeRoleWithSAML'
    { _arwsamlDurationSeconds :: !(Maybe Nat)
    , _arwsamlPolicy          :: !(Maybe Text)
    , _arwsamlRoleARN         :: !Text
    , _arwsamlPrincipalARN    :: !Text
    , _arwsamlSAMLAssertion   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AssumeRoleWithSAML' smart constructor.
assumeRoleWithSAML :: Text -> Text -> Text -> AssumeRoleWithSAML
assumeRoleWithSAML pRoleARN_ pPrincipalARN_ pSAMLAssertion_ =
    AssumeRoleWithSAML'
    { _arwsamlDurationSeconds = Nothing
    , _arwsamlPolicy = Nothing
    , _arwsamlRoleARN = pRoleARN_
    , _arwsamlPrincipalARN = pPrincipalARN_
    , _arwsamlSAMLAssertion = pSAMLAssertion_
    }

-- | The duration, in seconds, of the role session. The value can range from
-- 900 seconds (15 minutes) to 3600 seconds (1 hour). By default, the value
-- is set to 3600 seconds. An expiration can also be specified in the SAML
-- authentication response\'s @SessionNotOnOrAfter@ value. The actual
-- expiration time is whichever value is shorter.
--
-- The maximum duration for a session is 1 hour, and the minimum duration
-- is 15 minutes, even if values outside this range are specified.
arwsamlDurationSeconds :: Lens' AssumeRoleWithSAML (Maybe Natural)
arwsamlDurationSeconds = lens _arwsamlDurationSeconds (\ s a -> s{_arwsamlDurationSeconds = a}) . mapping _Nat;

-- | An IAM policy in JSON format.
--
-- The policy parameter is optional. If you pass a policy, the temporary
-- security credentials that are returned by the operation have the
-- permissions that are allowed by both the access policy of the role that
-- is being assumed, /__and__/ the policy that you pass. This gives you a
-- way to further restrict the permissions for the resulting temporary
-- security credentials. You cannot use the passed policy to grant
-- permissions that are in excess of those allowed by the access policy of
-- the role that is being assumed. For more information, see
-- <http://docs.aws.amazon.com/STS/latest/UsingSTS/permissions-assume-role.html Permissions for AssumeRoleWithSAML>
-- in /Using Temporary Security Credentials/.
--
-- The policy plain text must be 2048 bytes or shorter. However, an
-- internal conversion compresses it into a packed binary format with a
-- separate limit. The PackedPolicySize response element indicates by
-- percentage how close to the upper size limit the policy is, with 100%
-- equaling the maximum allowed size.
arwsamlPolicy :: Lens' AssumeRoleWithSAML (Maybe Text)
arwsamlPolicy = lens _arwsamlPolicy (\ s a -> s{_arwsamlPolicy = a});

-- | The Amazon Resource Name (ARN) of the role that the caller is assuming.
arwsamlRoleARN :: Lens' AssumeRoleWithSAML Text
arwsamlRoleARN = lens _arwsamlRoleARN (\ s a -> s{_arwsamlRoleARN = a});

-- | The Amazon Resource Name (ARN) of the SAML provider in IAM that
-- describes the IdP.
arwsamlPrincipalARN :: Lens' AssumeRoleWithSAML Text
arwsamlPrincipalARN = lens _arwsamlPrincipalARN (\ s a -> s{_arwsamlPrincipalARN = a});

-- | The base-64 encoded SAML authentication response provided by the IdP.
--
-- For more information, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/create-role-saml-IdP-tasks.html Configuring a Relying Party and Adding Claims>
-- in the /Using IAM/ guide.
arwsamlSAMLAssertion :: Lens' AssumeRoleWithSAML Text
arwsamlSAMLAssertion = lens _arwsamlSAMLAssertion (\ s a -> s{_arwsamlSAMLAssertion = a});

instance AWSRequest AssumeRoleWithSAML where
        type Sv AssumeRoleWithSAML = STS
        type Rs AssumeRoleWithSAML =
             AssumeRoleWithSAMLResponse
        request = postQuery
        response
          = receiveXMLWrapper "AssumeRoleWithSAMLResult"
              (\ s h x ->
                 AssumeRoleWithSAMLResponse' <$>
                   (x .@? "Audience") <*> (x .@? "Subject") <*>
                     (x .@? "PackedPolicySize")
                     <*> (x .@? "Credentials")
                     <*> (x .@? "SubjectType")
                     <*> (x .@? "NameQualifier")
                     <*> (x .@? "AssumedRoleUser")
                     <*> (x .@? "Issuer")
                     <*> (pure (fromEnum s)))

instance ToHeaders AssumeRoleWithSAML where
        toHeaders = const mempty

instance ToPath AssumeRoleWithSAML where
        toPath = const "/"

instance ToQuery AssumeRoleWithSAML where
        toQuery AssumeRoleWithSAML'{..}
          = mconcat
              ["Action" =: ("AssumeRoleWithSAML" :: ByteString),
               "Version" =: ("2011-06-15" :: ByteString),
               "DurationSeconds" =: _arwsamlDurationSeconds,
               "Policy" =: _arwsamlPolicy,
               "RoleArn" =: _arwsamlRoleARN,
               "PrincipalArn" =: _arwsamlPrincipalARN,
               "SAMLAssertion" =: _arwsamlSAMLAssertion]

-- | Contains the response to a successful AssumeRoleWithSAML request,
-- including temporary AWS credentials that can be used to make AWS
-- requests.
--
-- /See:/ 'assumeRoleWithSAMLResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'arwsamlrsAudience'
--
-- * 'arwsamlrsSubject'
--
-- * 'arwsamlrsPackedPolicySize'
--
-- * 'arwsamlrsCredentials'
--
-- * 'arwsamlrsSubjectType'
--
-- * 'arwsamlrsNameQualifier'
--
-- * 'arwsamlrsAssumedRoleUser'
--
-- * 'arwsamlrsIssuer'
--
-- * 'arwsamlrsStatus'
data AssumeRoleWithSAMLResponse = AssumeRoleWithSAMLResponse'
    { _arwsamlrsAudience         :: !(Maybe Text)
    , _arwsamlrsSubject          :: !(Maybe Text)
    , _arwsamlrsPackedPolicySize :: !(Maybe Nat)
    , _arwsamlrsCredentials      :: !(Maybe Credentials)
    , _arwsamlrsSubjectType      :: !(Maybe Text)
    , _arwsamlrsNameQualifier    :: !(Maybe Text)
    , _arwsamlrsAssumedRoleUser  :: !(Maybe AssumedRoleUser)
    , _arwsamlrsIssuer           :: !(Maybe Text)
    , _arwsamlrsStatus           :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AssumeRoleWithSAMLResponse' smart constructor.
assumeRoleWithSAMLResponse :: Int -> AssumeRoleWithSAMLResponse
assumeRoleWithSAMLResponse pStatus_ =
    AssumeRoleWithSAMLResponse'
    { _arwsamlrsAudience = Nothing
    , _arwsamlrsSubject = Nothing
    , _arwsamlrsPackedPolicySize = Nothing
    , _arwsamlrsCredentials = Nothing
    , _arwsamlrsSubjectType = Nothing
    , _arwsamlrsNameQualifier = Nothing
    , _arwsamlrsAssumedRoleUser = Nothing
    , _arwsamlrsIssuer = Nothing
    , _arwsamlrsStatus = pStatus_
    }

-- | The value of the @Recipient@ attribute of the @SubjectConfirmationData@
-- element of the SAML assertion.
arwsamlrsAudience :: Lens' AssumeRoleWithSAMLResponse (Maybe Text)
arwsamlrsAudience = lens _arwsamlrsAudience (\ s a -> s{_arwsamlrsAudience = a});

-- | The value of the @NameID@ element in the @Subject@ element of the SAML
-- assertion.
arwsamlrsSubject :: Lens' AssumeRoleWithSAMLResponse (Maybe Text)
arwsamlrsSubject = lens _arwsamlrsSubject (\ s a -> s{_arwsamlrsSubject = a});

-- | A percentage value that indicates the size of the policy in packed form.
-- The service rejects any policy with a packed size greater than 100
-- percent, which means the policy exceeded the allowed space.
arwsamlrsPackedPolicySize :: Lens' AssumeRoleWithSAMLResponse (Maybe Natural)
arwsamlrsPackedPolicySize = lens _arwsamlrsPackedPolicySize (\ s a -> s{_arwsamlrsPackedPolicySize = a}) . mapping _Nat;

-- | FIXME: Undocumented member.
arwsamlrsCredentials :: Lens' AssumeRoleWithSAMLResponse (Maybe Credentials)
arwsamlrsCredentials = lens _arwsamlrsCredentials (\ s a -> s{_arwsamlrsCredentials = a});

-- | The format of the name ID, as defined by the @Format@ attribute in the
-- @NameID@ element of the SAML assertion. Typical examples of the format
-- are @transient@ or @persistent@.
--
-- If the format includes the prefix
-- @urn:oasis:names:tc:SAML:2.0:nameid-format@, that prefix is removed. For
-- example, @urn:oasis:names:tc:SAML:2.0:nameid-format:transient@ is
-- returned as @transient@. If the format includes any other prefix, the
-- format is returned with no modifications.
arwsamlrsSubjectType :: Lens' AssumeRoleWithSAMLResponse (Maybe Text)
arwsamlrsSubjectType = lens _arwsamlrsSubjectType (\ s a -> s{_arwsamlrsSubjectType = a});

-- | A hash value based on the concatenation of the @Issuer@ response value,
-- the AWS account ID, and the friendly name (the last part of the ARN) of
-- the SAML provider in IAM. The combination of @NameQualifier@ and
-- @Subject@ can be used to uniquely identify a federated user.
--
-- The following pseudocode shows how the hash value is calculated:
--
-- @BASE64 ( SHA1 ( \"https:\/\/example.com\/saml\" + \"123456789012\" + \"\/MySAMLIdP\" ) )@
arwsamlrsNameQualifier :: Lens' AssumeRoleWithSAMLResponse (Maybe Text)
arwsamlrsNameQualifier = lens _arwsamlrsNameQualifier (\ s a -> s{_arwsamlrsNameQualifier = a});

-- | FIXME: Undocumented member.
arwsamlrsAssumedRoleUser :: Lens' AssumeRoleWithSAMLResponse (Maybe AssumedRoleUser)
arwsamlrsAssumedRoleUser = lens _arwsamlrsAssumedRoleUser (\ s a -> s{_arwsamlrsAssumedRoleUser = a});

-- | The value of the @Issuer@ element of the SAML assertion.
arwsamlrsIssuer :: Lens' AssumeRoleWithSAMLResponse (Maybe Text)
arwsamlrsIssuer = lens _arwsamlrsIssuer (\ s a -> s{_arwsamlrsIssuer = a});

-- | FIXME: Undocumented member.
arwsamlrsStatus :: Lens' AssumeRoleWithSAMLResponse Int
arwsamlrsStatus = lens _arwsamlrsStatus (\ s a -> s{_arwsamlrsStatus = a});
