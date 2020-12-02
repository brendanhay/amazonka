{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.RequestProgress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.RequestProgress where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal

-- | Container for specifying if periodic @QueryProgress@ messages should be sent.
--
--
--
-- /See:/ 'requestProgress' smart constructor.
newtype RequestProgress = RequestProgress'
  { _rpEnabled ::
      Maybe Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RequestProgress' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpEnabled' - Specifies whether periodic QueryProgress frames should be sent. Valid values: TRUE, FALSE. Default value: FALSE.
requestProgress ::
  RequestProgress
requestProgress = RequestProgress' {_rpEnabled = Nothing}

-- | Specifies whether periodic QueryProgress frames should be sent. Valid values: TRUE, FALSE. Default value: FALSE.
rpEnabled :: Lens' RequestProgress (Maybe Bool)
rpEnabled = lens _rpEnabled (\s a -> s {_rpEnabled = a})

instance Hashable RequestProgress

instance NFData RequestProgress

instance ToXML RequestProgress where
  toXML RequestProgress' {..} = mconcat ["Enabled" @= _rpEnabled]
