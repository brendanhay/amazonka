{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.STS.AssumeRoleWithSAML
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
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
-- specified when calling 'AssumeRoleWithSAML', which can be up to 3600
-- seconds (1 hour) or until the time specified in the SAML authentication
-- response\'s 'SessionNotOnOrAfter' value, whichever is shorter.
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
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_control-access_assumerole.html Permissions for AssumeRole, AssumeRoleWithSAML, and AssumeRoleWithWebIdentity>
-- in the /Using IAM/.
--
-- Before your application can call 'AssumeRoleWithSAML', you must
-- configure your SAML identity provider (IdP) to issue the claims required
-- by AWS. Additionally, you must use AWS Identity and Access Management
-- (IAM) to create a SAML provider entity in your AWS account that
-- represents your identity provider, and create an IAM role that specifies
-- this SAML provider in its trust policy.
--
-- Calling 'AssumeRoleWithSAML' does not require the use of AWS security
-- credentials. The identity of the caller is validated by using keys in
-- the metadata document that is uploaded for the SAML provider entity for
-- your identity provider.
--
-- For more information, see the following resources:
--
-- -   <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_providers_saml.html About SAML 2.0-based Federation>
--     in the /Using IAM/.
-- -   <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_providers_create_saml.html Creating SAML Identity Providers>
--     in the /Using IAM/.
-- -   <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_providers_create_saml_relying-party.html Configuring a Relying Party and Claims>
--     in the /Using IAM/.
-- -   <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_create_for-idp_saml.html Creating a Role for SAML 2.0 Federation>
--     in the /Using IAM/.
module Network.AWS.STS.AssumeRoleWithSAML
    (
    -- * Creating a Request
      assumeRoleWithSAML
    , AssumeRoleWithSAML
    -- * Request Lenses
    , arwsamlDurationSeconds
    , arwsamlPolicy
    , arwsamlRoleARN
    , arwsamlPrincipalARN
    , arwsamlSAMLAssertion

    -- * Destructuring the Response
    , assumeRoleWithSAMLResponse
    , AssumeRoleWithSAMLResponse
    -- * Response Lenses
    , arwsamlrsSubject
    , arwsamlrsAudience
    , arwsamlrsPackedPolicySize
    , arwsamlrsCredentials
    , arwsamlrsSubjectType
    , arwsamlrsNameQualifier
    , arwsamlrsAssumedRoleUser
    , arwsamlrsIssuer
    , arwsamlrsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.STS.Types
import           Network.AWS.STS.Types.Product

-- | /See:/ 'assumeRoleWithSAML' smart constructor.
data AssumeRoleWithSAML = AssumeRoleWithSAML'
    { _arwsamlDurationSeconds :: !(Maybe Nat)
    , _arwsamlPolicy          :: !(Maybe Text)
    , _arwsamlRoleARN         :: !Text
    , _arwsamlPrincipalARN    :: !Text
    , _arwsamlSAMLAssertion   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AssumeRoleWithSAML' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
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
assumeRoleWithSAML
    :: Text -- ^ 'arwsamlRoleARN'
    -> Text -- ^ 'arwsamlPrincipalARN'
    -> Text -- ^ 'arwsamlSAMLAssertion'
    -> AssumeRoleWithSAML
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
-- authentication response\'s 'SessionNotOnOrAfter' value. The actual
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
-- the role that is being assumed. For more information,
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_control-access_assumerole.html Permissions for AssumeRole, AssumeRoleWithSAML, and AssumeRoleWithWebIdentity>
-- in the /Using IAM/.
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
        type Rs AssumeRoleWithSAML =
             AssumeRoleWithSAMLResponse
        request = postQuery sts
        response
          = receiveXMLWrapper "AssumeRoleWithSAMLResult"
              (\ s h x ->
                 AssumeRoleWithSAMLResponse' <$>
                   (x .@? "Subject") <*> (x .@? "Audience") <*>
                     (x .@? "PackedPolicySize")
                     <*> (x .@? "Credentials")
                     <*> (x .@? "SubjectType")
                     <*> (x .@? "NameQualifier")
                     <*> (x .@? "AssumedRoleUser")
                     <*> (x .@? "Issuer")
                     <*> (pure (fromEnum s)))

instance Hashable AssumeRoleWithSAML

instance NFData AssumeRoleWithSAML

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

-- | Contains the response to a successful < AssumeRoleWithSAML> request,
-- including temporary AWS credentials that can be used to make AWS
-- requests.
--
-- /See:/ 'assumeRoleWithSAMLResponse' smart constructor.
data AssumeRoleWithSAMLResponse = AssumeRoleWithSAMLResponse'
    { _arwsamlrsSubject          :: !(Maybe Text)
    , _arwsamlrsAudience         :: !(Maybe Text)
    , _arwsamlrsPackedPolicySize :: !(Maybe Nat)
    , _arwsamlrsCredentials      :: !(Maybe Credentials)
    , _arwsamlrsSubjectType      :: !(Maybe Text)
    , _arwsamlrsNameQualifier    :: !(Maybe Text)
    , _arwsamlrsAssumedRoleUser  :: !(Maybe AssumedRoleUser)
    , _arwsamlrsIssuer           :: !(Maybe Text)
    , _arwsamlrsResponseStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AssumeRoleWithSAMLResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'arwsamlrsSubject'
--
-- * 'arwsamlrsAudience'
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
-- * 'arwsamlrsResponseStatus'
assumeRoleWithSAMLResponse
    :: Int -- ^ 'arwsamlrsResponseStatus'
    -> AssumeRoleWithSAMLResponse
assumeRoleWithSAMLResponse pResponseStatus_ =
    AssumeRoleWithSAMLResponse'
    { _arwsamlrsSubject = Nothing
    , _arwsamlrsAudience = Nothing
    , _arwsamlrsPackedPolicySize = Nothing
    , _arwsamlrsCredentials = Nothing
    , _arwsamlrsSubjectType = Nothing
    , _arwsamlrsNameQualifier = Nothing
    , _arwsamlrsAssumedRoleUser = Nothing
    , _arwsamlrsIssuer = Nothing
    , _arwsamlrsResponseStatus = pResponseStatus_
    }

-- | The value of the 'NameID' element in the 'Subject' element of the SAML
-- assertion.
arwsamlrsSubject :: Lens' AssumeRoleWithSAMLResponse (Maybe Text)
arwsamlrsSubject = lens _arwsamlrsSubject (\ s a -> s{_arwsamlrsSubject = a});

-- | The value of the 'Recipient' attribute of the 'SubjectConfirmationData'
-- element of the SAML assertion.
arwsamlrsAudience :: Lens' AssumeRoleWithSAMLResponse (Maybe Text)
arwsamlrsAudience = lens _arwsamlrsAudience (\ s a -> s{_arwsamlrsAudience = a});

-- | A percentage value that indicates the size of the policy in packed form.
-- The service rejects any policy with a packed size greater than 100
-- percent, which means the policy exceeded the allowed space.
arwsamlrsPackedPolicySize :: Lens' AssumeRoleWithSAMLResponse (Maybe Natural)
arwsamlrsPackedPolicySize = lens _arwsamlrsPackedPolicySize (\ s a -> s{_arwsamlrsPackedPolicySize = a}) . mapping _Nat;

-- | The temporary security credentials, which include an access key ID, a
-- secret access key, and a security (or session) token.
--
-- __Note:__ The size of the security token that STS APIs return is not
-- fixed. We strongly recommend that you make no assumptions about the
-- maximum size. As of this writing, the typical size is less than 4096
-- bytes, but that can vary. Also, future updates to AWS might require
-- larger sizes.
arwsamlrsCredentials :: Lens' AssumeRoleWithSAMLResponse (Maybe Credentials)
arwsamlrsCredentials = lens _arwsamlrsCredentials (\ s a -> s{_arwsamlrsCredentials = a});

-- | The format of the name ID, as defined by the 'Format' attribute in the
-- 'NameID' element of the SAML assertion. Typical examples of the format
-- are 'transient' or 'persistent'.
--
-- If the format includes the prefix
-- 'urn:oasis:names:tc:SAML:2.0:nameid-format', that prefix is removed. For
-- example, 'urn:oasis:names:tc:SAML:2.0:nameid-format:transient' is
-- returned as 'transient'. If the format includes any other prefix, the
-- format is returned with no modifications.
arwsamlrsSubjectType :: Lens' AssumeRoleWithSAMLResponse (Maybe Text)
arwsamlrsSubjectType = lens _arwsamlrsSubjectType (\ s a -> s{_arwsamlrsSubjectType = a});

-- | A hash value based on the concatenation of the 'Issuer' response value,
-- the AWS account ID, and the friendly name (the last part of the ARN) of
-- the SAML provider in IAM. The combination of 'NameQualifier' and
-- 'Subject' can be used to uniquely identify a federated user.
--
-- The following pseudocode shows how the hash value is calculated:
--
-- 'BASE64 ( SHA1 ( \"https:\/\/example.com\/saml\" + \"123456789012\" + \"\/MySAMLIdP\" ) )'
arwsamlrsNameQualifier :: Lens' AssumeRoleWithSAMLResponse (Maybe Text)
arwsamlrsNameQualifier = lens _arwsamlrsNameQualifier (\ s a -> s{_arwsamlrsNameQualifier = a});

-- | Undocumented member.
arwsamlrsAssumedRoleUser :: Lens' AssumeRoleWithSAMLResponse (Maybe AssumedRoleUser)
arwsamlrsAssumedRoleUser = lens _arwsamlrsAssumedRoleUser (\ s a -> s{_arwsamlrsAssumedRoleUser = a});

-- | The value of the 'Issuer' element of the SAML assertion.
arwsamlrsIssuer :: Lens' AssumeRoleWithSAMLResponse (Maybe Text)
arwsamlrsIssuer = lens _arwsamlrsIssuer (\ s a -> s{_arwsamlrsIssuer = a});

-- | The response status code.
arwsamlrsResponseStatus :: Lens' AssumeRoleWithSAMLResponse Int
arwsamlrsResponseStatus = lens _arwsamlrsResponseStatus (\ s a -> s{_arwsamlrsResponseStatus = a});
