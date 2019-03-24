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
-- Module      : Network.AWS.SageMaker.CreateModelPackage
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a model package that you can use to create Amazon SageMaker models or list on AWS Marketplace. Buyers can subscribe to model packages listed on AWS Marketplace to create models in Amazon SageMaker.
--
--
-- To create a model package by specifying a Docker container that contains your inference code and the Amazon S3 location of your model artifacts, provide values for @InferenceSpecification@ . To create a model from an algorithm resource that you created or subscribed to in AWS Marketplace, provide a value for @SourceAlgorithmSpecification@ .
--
module Network.AWS.SageMaker.CreateModelPackage
    (
    -- * Creating a Request
      createModelPackage
    , CreateModelPackage
    -- * Request Lenses
    , cmpSourceAlgorithmSpecification
    , cmpModelPackageDescription
    , cmpValidationSpecification
    , cmpInferenceSpecification
    , cmpCertifyForMarketplace
    , cmpModelPackageName

    -- * Destructuring the Response
    , createModelPackageResponse
    , CreateModelPackageResponse
    -- * Response Lenses
    , cmprsResponseStatus
    , cmprsModelPackageARN
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'createModelPackage' smart constructor.
data CreateModelPackage = CreateModelPackage'
  { _cmpSourceAlgorithmSpecification :: !(Maybe SourceAlgorithmSpecification)
  , _cmpModelPackageDescription :: !(Maybe Text)
  , _cmpValidationSpecification :: !(Maybe ModelPackageValidationSpecification)
  , _cmpInferenceSpecification :: !(Maybe InferenceSpecification)
  , _cmpCertifyForMarketplace :: !(Maybe Bool)
  , _cmpModelPackageName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateModelPackage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmpSourceAlgorithmSpecification' - Details about the algorithm that was used to create the model package.
--
-- * 'cmpModelPackageDescription' - A description of the model package.
--
-- * 'cmpValidationSpecification' - Specifies configurations for one or more transform jobs that Amazon SageMaker runs to test the model package.
--
-- * 'cmpInferenceSpecification' - Specifies details about inference jobs that can be run with models based on this model package, including the following:     * The Amazon ECR paths of containers that contain the inference code and model artifacts.     * The instance types that the model package supports for transform jobs and real-time endpoints used for inference.     * The input and output content formats that the model package supports for inference.
--
-- * 'cmpCertifyForMarketplace' - Whether to certify the model package for listing on AWS Marketplace.
--
-- * 'cmpModelPackageName' - The name of the model package. The name must have 1 to 63 characters. Valid characters are a-z, A-Z, 0-9, and - (hyphen).
createModelPackage
    :: Text -- ^ 'cmpModelPackageName'
    -> CreateModelPackage
createModelPackage pModelPackageName_ =
  CreateModelPackage'
    { _cmpSourceAlgorithmSpecification = Nothing
    , _cmpModelPackageDescription = Nothing
    , _cmpValidationSpecification = Nothing
    , _cmpInferenceSpecification = Nothing
    , _cmpCertifyForMarketplace = Nothing
    , _cmpModelPackageName = pModelPackageName_
    }


-- | Details about the algorithm that was used to create the model package.
cmpSourceAlgorithmSpecification :: Lens' CreateModelPackage (Maybe SourceAlgorithmSpecification)
cmpSourceAlgorithmSpecification = lens _cmpSourceAlgorithmSpecification (\ s a -> s{_cmpSourceAlgorithmSpecification = a})

-- | A description of the model package.
cmpModelPackageDescription :: Lens' CreateModelPackage (Maybe Text)
cmpModelPackageDescription = lens _cmpModelPackageDescription (\ s a -> s{_cmpModelPackageDescription = a})

-- | Specifies configurations for one or more transform jobs that Amazon SageMaker runs to test the model package.
cmpValidationSpecification :: Lens' CreateModelPackage (Maybe ModelPackageValidationSpecification)
cmpValidationSpecification = lens _cmpValidationSpecification (\ s a -> s{_cmpValidationSpecification = a})

-- | Specifies details about inference jobs that can be run with models based on this model package, including the following:     * The Amazon ECR paths of containers that contain the inference code and model artifacts.     * The instance types that the model package supports for transform jobs and real-time endpoints used for inference.     * The input and output content formats that the model package supports for inference.
cmpInferenceSpecification :: Lens' CreateModelPackage (Maybe InferenceSpecification)
cmpInferenceSpecification = lens _cmpInferenceSpecification (\ s a -> s{_cmpInferenceSpecification = a})

-- | Whether to certify the model package for listing on AWS Marketplace.
cmpCertifyForMarketplace :: Lens' CreateModelPackage (Maybe Bool)
cmpCertifyForMarketplace = lens _cmpCertifyForMarketplace (\ s a -> s{_cmpCertifyForMarketplace = a})

-- | The name of the model package. The name must have 1 to 63 characters. Valid characters are a-z, A-Z, 0-9, and - (hyphen).
cmpModelPackageName :: Lens' CreateModelPackage Text
cmpModelPackageName = lens _cmpModelPackageName (\ s a -> s{_cmpModelPackageName = a})

instance AWSRequest CreateModelPackage where
        type Rs CreateModelPackage =
             CreateModelPackageResponse
        request = postJSON sageMaker
        response
          = receiveJSON
              (\ s h x ->
                 CreateModelPackageResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "ModelPackageArn"))

instance Hashable CreateModelPackage where

instance NFData CreateModelPackage where

instance ToHeaders CreateModelPackage where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.CreateModelPackage" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateModelPackage where
        toJSON CreateModelPackage'{..}
          = object
              (catMaybes
                 [("SourceAlgorithmSpecification" .=) <$>
                    _cmpSourceAlgorithmSpecification,
                  ("ModelPackageDescription" .=) <$>
                    _cmpModelPackageDescription,
                  ("ValidationSpecification" .=) <$>
                    _cmpValidationSpecification,
                  ("InferenceSpecification" .=) <$>
                    _cmpInferenceSpecification,
                  ("CertifyForMarketplace" .=) <$>
                    _cmpCertifyForMarketplace,
                  Just ("ModelPackageName" .= _cmpModelPackageName)])

instance ToPath CreateModelPackage where
        toPath = const "/"

instance ToQuery CreateModelPackage where
        toQuery = const mempty

-- | /See:/ 'createModelPackageResponse' smart constructor.
data CreateModelPackageResponse = CreateModelPackageResponse'
  { _cmprsResponseStatus  :: !Int
  , _cmprsModelPackageARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateModelPackageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmprsResponseStatus' - -- | The response status code.
--
-- * 'cmprsModelPackageARN' - The Amazon Resource Name (ARN) of the new model package.
createModelPackageResponse
    :: Int -- ^ 'cmprsResponseStatus'
    -> Text -- ^ 'cmprsModelPackageARN'
    -> CreateModelPackageResponse
createModelPackageResponse pResponseStatus_ pModelPackageARN_ =
  CreateModelPackageResponse'
    { _cmprsResponseStatus = pResponseStatus_
    , _cmprsModelPackageARN = pModelPackageARN_
    }


-- | -- | The response status code.
cmprsResponseStatus :: Lens' CreateModelPackageResponse Int
cmprsResponseStatus = lens _cmprsResponseStatus (\ s a -> s{_cmprsResponseStatus = a})

-- | The Amazon Resource Name (ARN) of the new model package.
cmprsModelPackageARN :: Lens' CreateModelPackageResponse Text
cmprsModelPackageARN = lens _cmprsModelPackageARN (\ s a -> s{_cmprsModelPackageARN = a})

instance NFData CreateModelPackageResponse where
