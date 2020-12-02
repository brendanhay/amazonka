{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.Ulimit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.Ulimit where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The @ulimit@ settings to pass to the container.
--
--
--
-- /See:/ 'ulimit' smart constructor.
data Ulimit = Ulimit'
  { _uHardLimit :: !Int,
    _uName :: !Text,
    _uSoftLimit :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Ulimit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uHardLimit' - The hard limit for the @ulimit@ type.
--
-- * 'uName' - The @type@ of the @ulimit@ .
--
-- * 'uSoftLimit' - The soft limit for the @ulimit@ type.
ulimit ::
  -- | 'uHardLimit'
  Int ->
  -- | 'uName'
  Text ->
  -- | 'uSoftLimit'
  Int ->
  Ulimit
ulimit pHardLimit_ pName_ pSoftLimit_ =
  Ulimit'
    { _uHardLimit = pHardLimit_,
      _uName = pName_,
      _uSoftLimit = pSoftLimit_
    }

-- | The hard limit for the @ulimit@ type.
uHardLimit :: Lens' Ulimit Int
uHardLimit = lens _uHardLimit (\s a -> s {_uHardLimit = a})

-- | The @type@ of the @ulimit@ .
uName :: Lens' Ulimit Text
uName = lens _uName (\s a -> s {_uName = a})

-- | The soft limit for the @ulimit@ type.
uSoftLimit :: Lens' Ulimit Int
uSoftLimit = lens _uSoftLimit (\s a -> s {_uSoftLimit = a})

instance FromJSON Ulimit where
  parseJSON =
    withObject
      "Ulimit"
      ( \x ->
          Ulimit'
            <$> (x .: "hardLimit") <*> (x .: "name") <*> (x .: "softLimit")
      )

instance Hashable Ulimit

instance NFData Ulimit

instance ToJSON Ulimit where
  toJSON Ulimit' {..} =
    object
      ( catMaybes
          [ Just ("hardLimit" .= _uHardLimit),
            Just ("name" .= _uName),
            Just ("softLimit" .= _uSoftLimit)
          ]
      )
