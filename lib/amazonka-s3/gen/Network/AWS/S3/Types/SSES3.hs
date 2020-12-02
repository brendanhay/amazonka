{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.SSES3
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.SSES3 where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal

-- | Specifies the use of SSE-S3 to encrypt delivered inventory reports.
--
--
--
-- /See:/ 'sSES3' smart constructor.
data SSES3 = SSES3'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SSES3' with the minimum fields required to make a request.
sSES3 ::
  SSES3
sSES3 = SSES3'

instance FromXML SSES3 where
  parseXML = const (pure SSES3')

instance Hashable SSES3

instance NFData SSES3

instance ToXML SSES3 where
  toXML = const mempty
