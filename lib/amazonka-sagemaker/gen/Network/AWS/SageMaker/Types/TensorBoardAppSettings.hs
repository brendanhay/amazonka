{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TensorBoardAppSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TensorBoardAppSettings where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.ResourceSpec

-- | The TensorBoard app settings.
--
--
--
-- /See:/ 'tensorBoardAppSettings' smart constructor.
newtype TensorBoardAppSettings = TensorBoardAppSettings'
  { _tbasDefaultResourceSpec ::
      Maybe ResourceSpec
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TensorBoardAppSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tbasDefaultResourceSpec' - The default instance type and the Amazon Resource Name (ARN) of the SageMaker image created on the instance.
tensorBoardAppSettings ::
  TensorBoardAppSettings
tensorBoardAppSettings =
  TensorBoardAppSettings' {_tbasDefaultResourceSpec = Nothing}

-- | The default instance type and the Amazon Resource Name (ARN) of the SageMaker image created on the instance.
tbasDefaultResourceSpec :: Lens' TensorBoardAppSettings (Maybe ResourceSpec)
tbasDefaultResourceSpec = lens _tbasDefaultResourceSpec (\s a -> s {_tbasDefaultResourceSpec = a})

instance FromJSON TensorBoardAppSettings where
  parseJSON =
    withObject
      "TensorBoardAppSettings"
      (\x -> TensorBoardAppSettings' <$> (x .:? "DefaultResourceSpec"))

instance Hashable TensorBoardAppSettings

instance NFData TensorBoardAppSettings

instance ToJSON TensorBoardAppSettings where
  toJSON TensorBoardAppSettings' {..} =
    object
      ( catMaybes
          [("DefaultResourceSpec" .=) <$> _tbasDefaultResourceSpec]
      )
