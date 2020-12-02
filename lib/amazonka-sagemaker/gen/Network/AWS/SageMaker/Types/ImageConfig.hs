{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ImageConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ImageConfig where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.RepositoryAccessMode

-- | Specifies whether the model container is in Amazon ECR or a private Docker registry accessible from your Amazon Virtual Private Cloud (VPC).
--
--
--
-- /See:/ 'imageConfig' smart constructor.
newtype ImageConfig = ImageConfig'
  { _icRepositoryAccessMode ::
      RepositoryAccessMode
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ImageConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'icRepositoryAccessMode' - Set this to one of the following values:     * @Platform@ - The model image is hosted in Amazon ECR.     * @Vpc@ - The model image is hosted in a private Docker registry in your VPC.
imageConfig ::
  -- | 'icRepositoryAccessMode'
  RepositoryAccessMode ->
  ImageConfig
imageConfig pRepositoryAccessMode_ =
  ImageConfig' {_icRepositoryAccessMode = pRepositoryAccessMode_}

-- | Set this to one of the following values:     * @Platform@ - The model image is hosted in Amazon ECR.     * @Vpc@ - The model image is hosted in a private Docker registry in your VPC.
icRepositoryAccessMode :: Lens' ImageConfig RepositoryAccessMode
icRepositoryAccessMode = lens _icRepositoryAccessMode (\s a -> s {_icRepositoryAccessMode = a})

instance FromJSON ImageConfig where
  parseJSON =
    withObject
      "ImageConfig"
      (\x -> ImageConfig' <$> (x .: "RepositoryAccessMode"))

instance Hashable ImageConfig

instance NFData ImageConfig

instance ToJSON ImageConfig where
  toJSON ImageConfig' {..} =
    object
      ( catMaybes
          [Just ("RepositoryAccessMode" .= _icRepositoryAccessMode)]
      )
