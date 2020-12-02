{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.SupportedPlatform
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.SupportedPlatform where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal

-- | A list of supported platforms for orderable clusters.
--
--
--
-- /See:/ 'supportedPlatform' smart constructor.
newtype SupportedPlatform = SupportedPlatform'
  { _spName ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SupportedPlatform' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spName' -
supportedPlatform ::
  SupportedPlatform
supportedPlatform = SupportedPlatform' {_spName = Nothing}

-- |
spName :: Lens' SupportedPlatform (Maybe Text)
spName = lens _spName (\s a -> s {_spName = a})

instance FromXML SupportedPlatform where
  parseXML x = SupportedPlatform' <$> (x .@? "Name")

instance Hashable SupportedPlatform

instance NFData SupportedPlatform
