{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.Ulimit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.Ulimit where

import Network.AWS.ECS.Types.UlimitName
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The @ulimit@ settings to pass to the container.
--
--
--
-- /See:/ 'ulimit' smart constructor.
data Ulimit = Ulimit'
  { _uName :: !UlimitName,
    _uSoftLimit :: !Int,
    _uHardLimit :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Ulimit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uName' - The @type@ of the @ulimit@ .
--
-- * 'uSoftLimit' - The soft limit for the ulimit type.
--
-- * 'uHardLimit' - The hard limit for the ulimit type.
ulimit ::
  -- | 'uName'
  UlimitName ->
  -- | 'uSoftLimit'
  Int ->
  -- | 'uHardLimit'
  Int ->
  Ulimit
ulimit pName_ pSoftLimit_ pHardLimit_ =
  Ulimit'
    { _uName = pName_,
      _uSoftLimit = pSoftLimit_,
      _uHardLimit = pHardLimit_
    }

-- | The @type@ of the @ulimit@ .
uName :: Lens' Ulimit UlimitName
uName = lens _uName (\s a -> s {_uName = a})

-- | The soft limit for the ulimit type.
uSoftLimit :: Lens' Ulimit Int
uSoftLimit = lens _uSoftLimit (\s a -> s {_uSoftLimit = a})

-- | The hard limit for the ulimit type.
uHardLimit :: Lens' Ulimit Int
uHardLimit = lens _uHardLimit (\s a -> s {_uHardLimit = a})

instance FromJSON Ulimit where
  parseJSON =
    withObject
      "Ulimit"
      ( \x ->
          Ulimit'
            <$> (x .: "name") <*> (x .: "softLimit") <*> (x .: "hardLimit")
      )

instance Hashable Ulimit

instance NFData Ulimit

instance ToJSON Ulimit where
  toJSON Ulimit' {..} =
    object
      ( catMaybes
          [ Just ("name" .= _uName),
            Just ("softLimit" .= _uSoftLimit),
            Just ("hardLimit" .= _uHardLimit)
          ]
      )
