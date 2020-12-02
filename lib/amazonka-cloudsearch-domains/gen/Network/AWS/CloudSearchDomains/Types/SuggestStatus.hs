{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearchDomains.Types.SuggestStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearchDomains.Types.SuggestStatus where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains the resource id (@rid@ ) and the time it took to process the request (@timems@ ).
--
--
--
-- /See:/ 'suggestStatus' smart constructor.
data SuggestStatus = SuggestStatus'
  { _ssRid :: !(Maybe Text),
    _ssTimems :: !(Maybe Integer)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SuggestStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssRid' - The encrypted resource ID for the request.
--
-- * 'ssTimems' - How long it took to process the request, in milliseconds.
suggestStatus ::
  SuggestStatus
suggestStatus =
  SuggestStatus' {_ssRid = Nothing, _ssTimems = Nothing}

-- | The encrypted resource ID for the request.
ssRid :: Lens' SuggestStatus (Maybe Text)
ssRid = lens _ssRid (\s a -> s {_ssRid = a})

-- | How long it took to process the request, in milliseconds.
ssTimems :: Lens' SuggestStatus (Maybe Integer)
ssTimems = lens _ssTimems (\s a -> s {_ssTimems = a})

instance FromJSON SuggestStatus where
  parseJSON =
    withObject
      "SuggestStatus"
      (\x -> SuggestStatus' <$> (x .:? "rid") <*> (x .:? "timems"))

instance Hashable SuggestStatus

instance NFData SuggestStatus
