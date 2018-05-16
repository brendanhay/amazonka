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
-- Module      : Network.AWS.IoT.GetPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified policy with the policy document of the default version.
--
--
module Network.AWS.IoT.GetPolicy
    (
    -- * Creating a Request
      getPolicy
    , GetPolicy
    -- * Request Lenses
    , gpPolicyName

    -- * Destructuring the Response
    , getPolicyResponse
    , GetPolicyResponse
    -- * Response Lenses
    , gprsLastModifiedDate
    , gprsPolicyName
    , gprsPolicyDocument
    , gprsDefaultVersionId
    , gprsPolicyARN
    , gprsCreationDate
    , gprsGenerationId
    , gprsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the GetPolicy operation.
--
--
--
-- /See:/ 'getPolicy' smart constructor.
newtype GetPolicy = GetPolicy'
  { _gpPolicyName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpPolicyName' - The name of the policy.
getPolicy
    :: Text -- ^ 'gpPolicyName'
    -> GetPolicy
getPolicy pPolicyName_ = GetPolicy' {_gpPolicyName = pPolicyName_}


-- | The name of the policy.
gpPolicyName :: Lens' GetPolicy Text
gpPolicyName = lens _gpPolicyName (\ s a -> s{_gpPolicyName = a})

instance AWSRequest GetPolicy where
        type Rs GetPolicy = GetPolicyResponse
        request = get ioT
        response
          = receiveJSON
              (\ s h x ->
                 GetPolicyResponse' <$>
                   (x .?> "lastModifiedDate") <*> (x .?> "policyName")
                     <*> (x .?> "policyDocument")
                     <*> (x .?> "defaultVersionId")
                     <*> (x .?> "policyArn")
                     <*> (x .?> "creationDate")
                     <*> (x .?> "generationId")
                     <*> (pure (fromEnum s)))

instance Hashable GetPolicy where

instance NFData GetPolicy where

instance ToHeaders GetPolicy where
        toHeaders = const mempty

instance ToPath GetPolicy where
        toPath GetPolicy'{..}
          = mconcat ["/policies/", toBS _gpPolicyName]

instance ToQuery GetPolicy where
        toQuery = const mempty

-- | The output from the GetPolicy operation.
--
--
--
-- /See:/ 'getPolicyResponse' smart constructor.
data GetPolicyResponse = GetPolicyResponse'
  { _gprsLastModifiedDate :: !(Maybe POSIX)
  , _gprsPolicyName       :: !(Maybe Text)
  , _gprsPolicyDocument   :: !(Maybe Text)
  , _gprsDefaultVersionId :: !(Maybe Text)
  , _gprsPolicyARN        :: !(Maybe Text)
  , _gprsCreationDate     :: !(Maybe POSIX)
  , _gprsGenerationId     :: !(Maybe Text)
  , _gprsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gprsLastModifiedDate' - The date the policy was last modified.
--
-- * 'gprsPolicyName' - The policy name.
--
-- * 'gprsPolicyDocument' - The JSON document that describes the policy.
--
-- * 'gprsDefaultVersionId' - The default policy version ID.
--
-- * 'gprsPolicyARN' - The policy ARN.
--
-- * 'gprsCreationDate' - The date the policy was created.
--
-- * 'gprsGenerationId' - The generation ID of the policy.
--
-- * 'gprsResponseStatus' - -- | The response status code.
getPolicyResponse
    :: Int -- ^ 'gprsResponseStatus'
    -> GetPolicyResponse
getPolicyResponse pResponseStatus_ =
  GetPolicyResponse'
    { _gprsLastModifiedDate = Nothing
    , _gprsPolicyName = Nothing
    , _gprsPolicyDocument = Nothing
    , _gprsDefaultVersionId = Nothing
    , _gprsPolicyARN = Nothing
    , _gprsCreationDate = Nothing
    , _gprsGenerationId = Nothing
    , _gprsResponseStatus = pResponseStatus_
    }


-- | The date the policy was last modified.
gprsLastModifiedDate :: Lens' GetPolicyResponse (Maybe UTCTime)
gprsLastModifiedDate = lens _gprsLastModifiedDate (\ s a -> s{_gprsLastModifiedDate = a}) . mapping _Time

-- | The policy name.
gprsPolicyName :: Lens' GetPolicyResponse (Maybe Text)
gprsPolicyName = lens _gprsPolicyName (\ s a -> s{_gprsPolicyName = a})

-- | The JSON document that describes the policy.
gprsPolicyDocument :: Lens' GetPolicyResponse (Maybe Text)
gprsPolicyDocument = lens _gprsPolicyDocument (\ s a -> s{_gprsPolicyDocument = a})

-- | The default policy version ID.
gprsDefaultVersionId :: Lens' GetPolicyResponse (Maybe Text)
gprsDefaultVersionId = lens _gprsDefaultVersionId (\ s a -> s{_gprsDefaultVersionId = a})

-- | The policy ARN.
gprsPolicyARN :: Lens' GetPolicyResponse (Maybe Text)
gprsPolicyARN = lens _gprsPolicyARN (\ s a -> s{_gprsPolicyARN = a})

-- | The date the policy was created.
gprsCreationDate :: Lens' GetPolicyResponse (Maybe UTCTime)
gprsCreationDate = lens _gprsCreationDate (\ s a -> s{_gprsCreationDate = a}) . mapping _Time

-- | The generation ID of the policy.
gprsGenerationId :: Lens' GetPolicyResponse (Maybe Text)
gprsGenerationId = lens _gprsGenerationId (\ s a -> s{_gprsGenerationId = a})

-- | -- | The response status code.
gprsResponseStatus :: Lens' GetPolicyResponse Int
gprsResponseStatus = lens _gprsResponseStatus (\ s a -> s{_gprsResponseStatus = a})

instance NFData GetPolicyResponse where
