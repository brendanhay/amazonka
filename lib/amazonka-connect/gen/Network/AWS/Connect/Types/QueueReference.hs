{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.QueueReference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.QueueReference where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about a queue resource for which metrics are returned.
--
--
--
-- /See:/ 'queueReference' smart constructor.
data QueueReference = QueueReference'
  { _qrARN :: !(Maybe Text),
    _qrId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'QueueReference' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qrARN' - The Amazon Resource Name (ARN) of the queue.
--
-- * 'qrId' - The identifier of the queue.
queueReference ::
  QueueReference
queueReference = QueueReference' {_qrARN = Nothing, _qrId = Nothing}

-- | The Amazon Resource Name (ARN) of the queue.
qrARN :: Lens' QueueReference (Maybe Text)
qrARN = lens _qrARN (\s a -> s {_qrARN = a})

-- | The identifier of the queue.
qrId :: Lens' QueueReference (Maybe Text)
qrId = lens _qrId (\s a -> s {_qrId = a})

instance FromJSON QueueReference where
  parseJSON =
    withObject
      "QueueReference"
      (\x -> QueueReference' <$> (x .:? "Arn") <*> (x .:? "Id"))

instance Hashable QueueReference

instance NFData QueueReference
