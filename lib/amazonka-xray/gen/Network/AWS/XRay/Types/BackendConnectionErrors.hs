{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.BackendConnectionErrors
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.BackendConnectionErrors where

import Network.AWS.Lens
import Network.AWS.Prelude

-- |
--
--
--
-- /See:/ 'backendConnectionErrors' smart constructor.
data BackendConnectionErrors = BackendConnectionErrors'
  { _bceOtherCount ::
      !(Maybe Int),
    _bceTimeoutCount :: !(Maybe Int),
    _bceHTTPCode5XXCount :: !(Maybe Int),
    _bceConnectionRefusedCount :: !(Maybe Int),
    _bceHTTPCode4XXCount :: !(Maybe Int),
    _bceUnknownHostCount :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BackendConnectionErrors' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bceOtherCount' -
--
-- * 'bceTimeoutCount' -
--
-- * 'bceHTTPCode5XXCount' -
--
-- * 'bceConnectionRefusedCount' -
--
-- * 'bceHTTPCode4XXCount' -
--
-- * 'bceUnknownHostCount' -
backendConnectionErrors ::
  BackendConnectionErrors
backendConnectionErrors =
  BackendConnectionErrors'
    { _bceOtherCount = Nothing,
      _bceTimeoutCount = Nothing,
      _bceHTTPCode5XXCount = Nothing,
      _bceConnectionRefusedCount = Nothing,
      _bceHTTPCode4XXCount = Nothing,
      _bceUnknownHostCount = Nothing
    }

-- |
bceOtherCount :: Lens' BackendConnectionErrors (Maybe Int)
bceOtherCount = lens _bceOtherCount (\s a -> s {_bceOtherCount = a})

-- |
bceTimeoutCount :: Lens' BackendConnectionErrors (Maybe Int)
bceTimeoutCount = lens _bceTimeoutCount (\s a -> s {_bceTimeoutCount = a})

-- |
bceHTTPCode5XXCount :: Lens' BackendConnectionErrors (Maybe Int)
bceHTTPCode5XXCount = lens _bceHTTPCode5XXCount (\s a -> s {_bceHTTPCode5XXCount = a})

-- |
bceConnectionRefusedCount :: Lens' BackendConnectionErrors (Maybe Int)
bceConnectionRefusedCount = lens _bceConnectionRefusedCount (\s a -> s {_bceConnectionRefusedCount = a})

-- |
bceHTTPCode4XXCount :: Lens' BackendConnectionErrors (Maybe Int)
bceHTTPCode4XXCount = lens _bceHTTPCode4XXCount (\s a -> s {_bceHTTPCode4XXCount = a})

-- |
bceUnknownHostCount :: Lens' BackendConnectionErrors (Maybe Int)
bceUnknownHostCount = lens _bceUnknownHostCount (\s a -> s {_bceUnknownHostCount = a})

instance Hashable BackendConnectionErrors

instance NFData BackendConnectionErrors

instance ToJSON BackendConnectionErrors where
  toJSON BackendConnectionErrors' {..} =
    object
      ( catMaybes
          [ ("OtherCount" .=) <$> _bceOtherCount,
            ("TimeoutCount" .=) <$> _bceTimeoutCount,
            ("HTTPCode5XXCount" .=) <$> _bceHTTPCode5XXCount,
            ("ConnectionRefusedCount" .=) <$> _bceConnectionRefusedCount,
            ("HTTPCode4XXCount" .=) <$> _bceHTTPCode4XXCount,
            ("UnknownHostCount" .=) <$> _bceUnknownHostCount
          ]
      )
