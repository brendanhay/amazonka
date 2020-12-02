{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.Types.DeleteMessageBatchResultEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SQS.Types.DeleteMessageBatchResultEntry where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Encloses the @Id@ of an entry in @'DeleteMessageBatch' .@
--
--
--
-- /See:/ 'deleteMessageBatchResultEntry' smart constructor.
newtype DeleteMessageBatchResultEntry = DeleteMessageBatchResultEntry'
  { _dId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteMessageBatchResultEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dId' - Represents a successfully deleted message.
deleteMessageBatchResultEntry ::
  -- | 'dId'
  Text ->
  DeleteMessageBatchResultEntry
deleteMessageBatchResultEntry pId_ =
  DeleteMessageBatchResultEntry' {_dId = pId_}

-- | Represents a successfully deleted message.
dId :: Lens' DeleteMessageBatchResultEntry Text
dId = lens _dId (\s a -> s {_dId = a})

instance FromXML DeleteMessageBatchResultEntry where
  parseXML x = DeleteMessageBatchResultEntry' <$> (x .@ "Id")

instance Hashable DeleteMessageBatchResultEntry

instance NFData DeleteMessageBatchResultEntry
