{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.EnclaveOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.EnclaveOptions where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Indicates whether the instance is enabled for AWS Nitro Enclaves.
--
--
--
-- /See:/ 'enclaveOptions' smart constructor.
newtype EnclaveOptions = EnclaveOptions' {_eoEnabled :: Maybe Bool}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EnclaveOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eoEnabled' - If this parameter is set to @true@ , the instance is enabled for AWS Nitro Enclaves; otherwise, it is not enabled for AWS Nitro Enclaves.
enclaveOptions ::
  EnclaveOptions
enclaveOptions = EnclaveOptions' {_eoEnabled = Nothing}

-- | If this parameter is set to @true@ , the instance is enabled for AWS Nitro Enclaves; otherwise, it is not enabled for AWS Nitro Enclaves.
eoEnabled :: Lens' EnclaveOptions (Maybe Bool)
eoEnabled = lens _eoEnabled (\s a -> s {_eoEnabled = a})

instance FromXML EnclaveOptions where
  parseXML x = EnclaveOptions' <$> (x .@? "enabled")

instance Hashable EnclaveOptions

instance NFData EnclaveOptions
