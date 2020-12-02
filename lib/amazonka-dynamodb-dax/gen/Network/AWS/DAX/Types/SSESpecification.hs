{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.Types.SSESpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DAX.Types.SSESpecification where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the settings used to enable server-side encryption.
--
--
--
-- /See:/ 'sSESpecification' smart constructor.
newtype SSESpecification = SSESpecification' {_ssesEnabled :: Bool}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SSESpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssesEnabled' - Indicates whether server-side encryption is enabled (true) or disabled (false) on the cluster.
sSESpecification ::
  -- | 'ssesEnabled'
  Bool ->
  SSESpecification
sSESpecification pEnabled_ =
  SSESpecification' {_ssesEnabled = pEnabled_}

-- | Indicates whether server-side encryption is enabled (true) or disabled (false) on the cluster.
ssesEnabled :: Lens' SSESpecification Bool
ssesEnabled = lens _ssesEnabled (\s a -> s {_ssesEnabled = a})

instance Hashable SSESpecification

instance NFData SSESpecification

instance ToJSON SSESpecification where
  toJSON SSESpecification' {..} =
    object (catMaybes [Just ("Enabled" .= _ssesEnabled)])
