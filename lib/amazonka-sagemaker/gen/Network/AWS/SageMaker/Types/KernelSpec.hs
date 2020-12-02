{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.KernelSpec
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.KernelSpec where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The specification of a Jupyter kernel.
--
--
--
-- /See:/ 'kernelSpec' smart constructor.
data KernelSpec = KernelSpec'
  { _ksDisplayName :: !(Maybe Text),
    _ksName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'KernelSpec' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ksDisplayName' - The display name of the kernel.
--
-- * 'ksName' - The name of the kernel.
kernelSpec ::
  -- | 'ksName'
  Text ->
  KernelSpec
kernelSpec pName_ =
  KernelSpec' {_ksDisplayName = Nothing, _ksName = pName_}

-- | The display name of the kernel.
ksDisplayName :: Lens' KernelSpec (Maybe Text)
ksDisplayName = lens _ksDisplayName (\s a -> s {_ksDisplayName = a})

-- | The name of the kernel.
ksName :: Lens' KernelSpec Text
ksName = lens _ksName (\s a -> s {_ksName = a})

instance FromJSON KernelSpec where
  parseJSON =
    withObject
      "KernelSpec"
      (\x -> KernelSpec' <$> (x .:? "DisplayName") <*> (x .: "Name"))

instance Hashable KernelSpec

instance NFData KernelSpec

instance ToJSON KernelSpec where
  toJSON KernelSpec' {..} =
    object
      ( catMaybes
          [("DisplayName" .=) <$> _ksDisplayName, Just ("Name" .= _ksName)]
      )
