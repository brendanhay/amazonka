{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.CustomImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.CustomImage where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A custom SageMaker image. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/studio-byoi.html Bring your own SageMaker image> .
--
--
--
-- /See:/ 'customImage' smart constructor.
data CustomImage = CustomImage'
  { _ciImageVersionNumber ::
      !(Maybe Nat),
    _ciImageName :: !Text,
    _ciAppImageConfigName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CustomImage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ciImageVersionNumber' - The version number of the CustomImage.
--
-- * 'ciImageName' - The name of the CustomImage. Must be unique to your account.
--
-- * 'ciAppImageConfigName' - The name of the AppImageConfig.
customImage ::
  -- | 'ciImageName'
  Text ->
  -- | 'ciAppImageConfigName'
  Text ->
  CustomImage
customImage pImageName_ pAppImageConfigName_ =
  CustomImage'
    { _ciImageVersionNumber = Nothing,
      _ciImageName = pImageName_,
      _ciAppImageConfigName = pAppImageConfigName_
    }

-- | The version number of the CustomImage.
ciImageVersionNumber :: Lens' CustomImage (Maybe Natural)
ciImageVersionNumber = lens _ciImageVersionNumber (\s a -> s {_ciImageVersionNumber = a}) . mapping _Nat

-- | The name of the CustomImage. Must be unique to your account.
ciImageName :: Lens' CustomImage Text
ciImageName = lens _ciImageName (\s a -> s {_ciImageName = a})

-- | The name of the AppImageConfig.
ciAppImageConfigName :: Lens' CustomImage Text
ciAppImageConfigName = lens _ciAppImageConfigName (\s a -> s {_ciAppImageConfigName = a})

instance FromJSON CustomImage where
  parseJSON =
    withObject
      "CustomImage"
      ( \x ->
          CustomImage'
            <$> (x .:? "ImageVersionNumber")
            <*> (x .: "ImageName")
            <*> (x .: "AppImageConfigName")
      )

instance Hashable CustomImage

instance NFData CustomImage

instance ToJSON CustomImage where
  toJSON CustomImage' {..} =
    object
      ( catMaybes
          [ ("ImageVersionNumber" .=) <$> _ciImageVersionNumber,
            Just ("ImageName" .= _ciImageName),
            Just ("AppImageConfigName" .= _ciAppImageConfigName)
          ]
      )
