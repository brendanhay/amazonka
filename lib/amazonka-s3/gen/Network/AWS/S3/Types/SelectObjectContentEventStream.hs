{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.SelectObjectContentEventStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.SelectObjectContentEventStream where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.ContinuationEvent
import Network.AWS.S3.Types.EndEvent
import Network.AWS.S3.Types.ProgressEvent
import Network.AWS.S3.Types.RecordsEvent
import Network.AWS.S3.Types.StatsEvent

-- | The container for selecting objects from a content event stream.
--
--
--
-- /See:/ 'selectObjectContentEventStream' smart constructor.
data SelectObjectContentEventStream = SelectObjectContentEventStream'
  { _socesProgress ::
      !(Maybe ProgressEvent),
    _socesRecords ::
      !(Maybe RecordsEvent),
    _socesCont ::
      !(Maybe ContinuationEvent),
    _socesStats ::
      !(Maybe StatsEvent),
    _socesEnd ::
      !(Maybe EndEvent)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SelectObjectContentEventStream' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'socesProgress' - The Progress Event.
--
-- * 'socesRecords' - The Records Event.
--
-- * 'socesCont' - The Continuation Event.
--
-- * 'socesStats' - The Stats Event.
--
-- * 'socesEnd' - The End Event.
selectObjectContentEventStream ::
  SelectObjectContentEventStream
selectObjectContentEventStream =
  SelectObjectContentEventStream'
    { _socesProgress = Nothing,
      _socesRecords = Nothing,
      _socesCont = Nothing,
      _socesStats = Nothing,
      _socesEnd = Nothing
    }

-- | The Progress Event.
socesProgress :: Lens' SelectObjectContentEventStream (Maybe ProgressEvent)
socesProgress = lens _socesProgress (\s a -> s {_socesProgress = a})

-- | The Records Event.
socesRecords :: Lens' SelectObjectContentEventStream (Maybe RecordsEvent)
socesRecords = lens _socesRecords (\s a -> s {_socesRecords = a})

-- | The Continuation Event.
socesCont :: Lens' SelectObjectContentEventStream (Maybe ContinuationEvent)
socesCont = lens _socesCont (\s a -> s {_socesCont = a})

-- | The Stats Event.
socesStats :: Lens' SelectObjectContentEventStream (Maybe StatsEvent)
socesStats = lens _socesStats (\s a -> s {_socesStats = a})

-- | The End Event.
socesEnd :: Lens' SelectObjectContentEventStream (Maybe EndEvent)
socesEnd = lens _socesEnd (\s a -> s {_socesEnd = a})

instance FromXML SelectObjectContentEventStream where
  parseXML x =
    SelectObjectContentEventStream'
      <$> (x .@? "Progress")
      <*> (x .@? "Records")
      <*> (x .@? "Cont")
      <*> (x .@? "Stats")
      <*> (x .@? "End")

instance Hashable SelectObjectContentEventStream

instance NFData SelectObjectContentEventStream
