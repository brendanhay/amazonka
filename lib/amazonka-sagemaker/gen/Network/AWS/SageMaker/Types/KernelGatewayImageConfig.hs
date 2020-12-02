{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.KernelGatewayImageConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.KernelGatewayImageConfig where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.FileSystemConfig
import Network.AWS.SageMaker.Types.KernelSpec

-- | The configuration for the file system and kernels in a SageMaker image running as a KernelGateway app.
--
--
--
-- /See:/ 'kernelGatewayImageConfig' smart constructor.
data KernelGatewayImageConfig = KernelGatewayImageConfig'
  { _kgicFileSystemConfig ::
      !(Maybe FileSystemConfig),
    _kgicKernelSpecs :: !(List1 KernelSpec)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'KernelGatewayImageConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kgicFileSystemConfig' - The Amazon Elastic File System (EFS) storage configuration for a SageMaker image.
--
-- * 'kgicKernelSpecs' - The specification of the Jupyter kernels in the image.
kernelGatewayImageConfig ::
  -- | 'kgicKernelSpecs'
  NonEmpty KernelSpec ->
  KernelGatewayImageConfig
kernelGatewayImageConfig pKernelSpecs_ =
  KernelGatewayImageConfig'
    { _kgicFileSystemConfig = Nothing,
      _kgicKernelSpecs = _List1 # pKernelSpecs_
    }

-- | The Amazon Elastic File System (EFS) storage configuration for a SageMaker image.
kgicFileSystemConfig :: Lens' KernelGatewayImageConfig (Maybe FileSystemConfig)
kgicFileSystemConfig = lens _kgicFileSystemConfig (\s a -> s {_kgicFileSystemConfig = a})

-- | The specification of the Jupyter kernels in the image.
kgicKernelSpecs :: Lens' KernelGatewayImageConfig (NonEmpty KernelSpec)
kgicKernelSpecs = lens _kgicKernelSpecs (\s a -> s {_kgicKernelSpecs = a}) . _List1

instance FromJSON KernelGatewayImageConfig where
  parseJSON =
    withObject
      "KernelGatewayImageConfig"
      ( \x ->
          KernelGatewayImageConfig'
            <$> (x .:? "FileSystemConfig") <*> (x .: "KernelSpecs")
      )

instance Hashable KernelGatewayImageConfig

instance NFData KernelGatewayImageConfig

instance ToJSON KernelGatewayImageConfig where
  toJSON KernelGatewayImageConfig' {..} =
    object
      ( catMaybes
          [ ("FileSystemConfig" .=) <$> _kgicFileSystemConfig,
            Just ("KernelSpecs" .= _kgicKernelSpecs)
          ]
      )
