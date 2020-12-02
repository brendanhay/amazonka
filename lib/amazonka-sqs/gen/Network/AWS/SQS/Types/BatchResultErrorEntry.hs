{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.Types.BatchResultErrorEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SQS.Types.BatchResultErrorEntry where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Gives a detailed description of the result of an action on each entry in the request.
--
--
--
-- /See:/ 'batchResultErrorEntry' smart constructor.
data BatchResultErrorEntry = BatchResultErrorEntry'
  { _breeMessage ::
      !(Maybe Text),
    _breeId :: !Text,
    _breeSenderFault :: !Bool,
    _breeCode :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchResultErrorEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'breeMessage' - A message explaining why the action failed on this entry.
--
-- * 'breeId' - The @Id@ of an entry in a batch request.
--
-- * 'breeSenderFault' - Specifies whether the error happened due to the caller of the batch API action.
--
-- * 'breeCode' - An error code representing why the action failed on this entry.
batchResultErrorEntry ::
  -- | 'breeId'
  Text ->
  -- | 'breeSenderFault'
  Bool ->
  -- | 'breeCode'
  Text ->
  BatchResultErrorEntry
batchResultErrorEntry pId_ pSenderFault_ pCode_ =
  BatchResultErrorEntry'
    { _breeMessage = Nothing,
      _breeId = pId_,
      _breeSenderFault = pSenderFault_,
      _breeCode = pCode_
    }

-- | A message explaining why the action failed on this entry.
breeMessage :: Lens' BatchResultErrorEntry (Maybe Text)
breeMessage = lens _breeMessage (\s a -> s {_breeMessage = a})

-- | The @Id@ of an entry in a batch request.
breeId :: Lens' BatchResultErrorEntry Text
breeId = lens _breeId (\s a -> s {_breeId = a})

-- | Specifies whether the error happened due to the caller of the batch API action.
breeSenderFault :: Lens' BatchResultErrorEntry Bool
breeSenderFault = lens _breeSenderFault (\s a -> s {_breeSenderFault = a})

-- | An error code representing why the action failed on this entry.
breeCode :: Lens' BatchResultErrorEntry Text
breeCode = lens _breeCode (\s a -> s {_breeCode = a})

instance FromXML BatchResultErrorEntry where
  parseXML x =
    BatchResultErrorEntry'
      <$> (x .@? "Message")
      <*> (x .@ "Id")
      <*> (x .@ "SenderFault")
      <*> (x .@ "Code")

instance Hashable BatchResultErrorEntry

instance NFData BatchResultErrorEntry
