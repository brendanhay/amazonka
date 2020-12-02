{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DescribeModelPackage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a description of the specified model package, which is used to create Amazon SageMaker models or list them on AWS Marketplace.
--
--
-- To create models in Amazon SageMaker, buyers can subscribe to model packages listed on AWS Marketplace.
module Network.AWS.SageMaker.DescribeModelPackage
  ( -- * Creating a Request
    describeModelPackage,
    DescribeModelPackage,

    -- * Request Lenses
    dModelPackageName,

    -- * Destructuring the Response
    describeModelPackageResponse,
    DescribeModelPackageResponse,

    -- * Response Lenses
    dmprsSourceAlgorithmSpecification,
    dmprsModelPackageDescription,
    dmprsValidationSpecification,
    dmprsInferenceSpecification,
    dmprsCertifyForMarketplace,
    dmprsResponseStatus,
    dmprsModelPackageName,
    dmprsModelPackageARN,
    dmprsCreationTime,
    dmprsModelPackageStatus,
    dmprsModelPackageStatusDetails,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'describeModelPackage' smart constructor.
newtype DescribeModelPackage = DescribeModelPackage'
  { _dModelPackageName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeModelPackage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dModelPackageName' - The name of the model package to describe.
describeModelPackage ::
  -- | 'dModelPackageName'
  Text ->
  DescribeModelPackage
describeModelPackage pModelPackageName_ =
  DescribeModelPackage' {_dModelPackageName = pModelPackageName_}

-- | The name of the model package to describe.
dModelPackageName :: Lens' DescribeModelPackage Text
dModelPackageName = lens _dModelPackageName (\s a -> s {_dModelPackageName = a})

instance AWSRequest DescribeModelPackage where
  type Rs DescribeModelPackage = DescribeModelPackageResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          DescribeModelPackageResponse'
            <$> (x .?> "SourceAlgorithmSpecification")
            <*> (x .?> "ModelPackageDescription")
            <*> (x .?> "ValidationSpecification")
            <*> (x .?> "InferenceSpecification")
            <*> (x .?> "CertifyForMarketplace")
            <*> (pure (fromEnum s))
            <*> (x .:> "ModelPackageName")
            <*> (x .:> "ModelPackageArn")
            <*> (x .:> "CreationTime")
            <*> (x .:> "ModelPackageStatus")
            <*> (x .:> "ModelPackageStatusDetails")
      )

instance Hashable DescribeModelPackage

instance NFData DescribeModelPackage

instance ToHeaders DescribeModelPackage where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("SageMaker.DescribeModelPackage" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeModelPackage where
  toJSON DescribeModelPackage' {..} =
    object
      (catMaybes [Just ("ModelPackageName" .= _dModelPackageName)])

instance ToPath DescribeModelPackage where
  toPath = const "/"

instance ToQuery DescribeModelPackage where
  toQuery = const mempty

-- | /See:/ 'describeModelPackageResponse' smart constructor.
data DescribeModelPackageResponse = DescribeModelPackageResponse'
  { _dmprsSourceAlgorithmSpecification ::
      !( Maybe
           SourceAlgorithmSpecification
       ),
    _dmprsModelPackageDescription ::
      !(Maybe Text),
    _dmprsValidationSpecification ::
      !( Maybe
           ModelPackageValidationSpecification
       ),
    _dmprsInferenceSpecification ::
      !(Maybe InferenceSpecification),
    _dmprsCertifyForMarketplace ::
      !(Maybe Bool),
    _dmprsResponseStatus :: !Int,
    _dmprsModelPackageName :: !Text,
    _dmprsModelPackageARN :: !Text,
    _dmprsCreationTime :: !POSIX,
    _dmprsModelPackageStatus ::
      !ModelPackageStatus,
    _dmprsModelPackageStatusDetails ::
      !ModelPackageStatusDetails
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeModelPackageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmprsSourceAlgorithmSpecification' - Details about the algorithm that was used to create the model package.
--
-- * 'dmprsModelPackageDescription' - A brief summary of the model package.
--
-- * 'dmprsValidationSpecification' - Configurations for one or more transform jobs that Amazon SageMaker runs to test the model package.
--
-- * 'dmprsInferenceSpecification' - Details about inference jobs that can be run with models based on this model package.
--
-- * 'dmprsCertifyForMarketplace' - Whether the model package is certified for listing on AWS Marketplace.
--
-- * 'dmprsResponseStatus' - -- | The response status code.
--
-- * 'dmprsModelPackageName' - The name of the model package being described.
--
-- * 'dmprsModelPackageARN' - The Amazon Resource Name (ARN) of the model package.
--
-- * 'dmprsCreationTime' - A timestamp specifying when the model package was created.
--
-- * 'dmprsModelPackageStatus' - The current status of the model package.
--
-- * 'dmprsModelPackageStatusDetails' - Details about the current status of the model package.
describeModelPackageResponse ::
  -- | 'dmprsResponseStatus'
  Int ->
  -- | 'dmprsModelPackageName'
  Text ->
  -- | 'dmprsModelPackageARN'
  Text ->
  -- | 'dmprsCreationTime'
  UTCTime ->
  -- | 'dmprsModelPackageStatus'
  ModelPackageStatus ->
  -- | 'dmprsModelPackageStatusDetails'
  ModelPackageStatusDetails ->
  DescribeModelPackageResponse
describeModelPackageResponse
  pResponseStatus_
  pModelPackageName_
  pModelPackageARN_
  pCreationTime_
  pModelPackageStatus_
  pModelPackageStatusDetails_ =
    DescribeModelPackageResponse'
      { _dmprsSourceAlgorithmSpecification =
          Nothing,
        _dmprsModelPackageDescription = Nothing,
        _dmprsValidationSpecification = Nothing,
        _dmprsInferenceSpecification = Nothing,
        _dmprsCertifyForMarketplace = Nothing,
        _dmprsResponseStatus = pResponseStatus_,
        _dmprsModelPackageName = pModelPackageName_,
        _dmprsModelPackageARN = pModelPackageARN_,
        _dmprsCreationTime = _Time # pCreationTime_,
        _dmprsModelPackageStatus = pModelPackageStatus_,
        _dmprsModelPackageStatusDetails = pModelPackageStatusDetails_
      }

-- | Details about the algorithm that was used to create the model package.
dmprsSourceAlgorithmSpecification :: Lens' DescribeModelPackageResponse (Maybe SourceAlgorithmSpecification)
dmprsSourceAlgorithmSpecification = lens _dmprsSourceAlgorithmSpecification (\s a -> s {_dmprsSourceAlgorithmSpecification = a})

-- | A brief summary of the model package.
dmprsModelPackageDescription :: Lens' DescribeModelPackageResponse (Maybe Text)
dmprsModelPackageDescription = lens _dmprsModelPackageDescription (\s a -> s {_dmprsModelPackageDescription = a})

-- | Configurations for one or more transform jobs that Amazon SageMaker runs to test the model package.
dmprsValidationSpecification :: Lens' DescribeModelPackageResponse (Maybe ModelPackageValidationSpecification)
dmprsValidationSpecification = lens _dmprsValidationSpecification (\s a -> s {_dmprsValidationSpecification = a})

-- | Details about inference jobs that can be run with models based on this model package.
dmprsInferenceSpecification :: Lens' DescribeModelPackageResponse (Maybe InferenceSpecification)
dmprsInferenceSpecification = lens _dmprsInferenceSpecification (\s a -> s {_dmprsInferenceSpecification = a})

-- | Whether the model package is certified for listing on AWS Marketplace.
dmprsCertifyForMarketplace :: Lens' DescribeModelPackageResponse (Maybe Bool)
dmprsCertifyForMarketplace = lens _dmprsCertifyForMarketplace (\s a -> s {_dmprsCertifyForMarketplace = a})

-- | -- | The response status code.
dmprsResponseStatus :: Lens' DescribeModelPackageResponse Int
dmprsResponseStatus = lens _dmprsResponseStatus (\s a -> s {_dmprsResponseStatus = a})

-- | The name of the model package being described.
dmprsModelPackageName :: Lens' DescribeModelPackageResponse Text
dmprsModelPackageName = lens _dmprsModelPackageName (\s a -> s {_dmprsModelPackageName = a})

-- | The Amazon Resource Name (ARN) of the model package.
dmprsModelPackageARN :: Lens' DescribeModelPackageResponse Text
dmprsModelPackageARN = lens _dmprsModelPackageARN (\s a -> s {_dmprsModelPackageARN = a})

-- | A timestamp specifying when the model package was created.
dmprsCreationTime :: Lens' DescribeModelPackageResponse UTCTime
dmprsCreationTime = lens _dmprsCreationTime (\s a -> s {_dmprsCreationTime = a}) . _Time

-- | The current status of the model package.
dmprsModelPackageStatus :: Lens' DescribeModelPackageResponse ModelPackageStatus
dmprsModelPackageStatus = lens _dmprsModelPackageStatus (\s a -> s {_dmprsModelPackageStatus = a})

-- | Details about the current status of the model package.
dmprsModelPackageStatusDetails :: Lens' DescribeModelPackageResponse ModelPackageStatusDetails
dmprsModelPackageStatusDetails = lens _dmprsModelPackageStatusDetails (\s a -> s {_dmprsModelPackageStatusDetails = a})

instance NFData DescribeModelPackageResponse
