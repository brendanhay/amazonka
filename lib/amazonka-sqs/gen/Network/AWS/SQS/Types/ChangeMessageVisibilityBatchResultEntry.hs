{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.Types.ChangeMessageVisibilityBatchResultEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SQS.Types.ChangeMessageVisibilityBatchResultEntry where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Encloses the @Id@ of an entry in @'ChangeMessageVisibilityBatch' .@
--
--
--
-- /See:/ 'changeMessageVisibilityBatchResultEntry' smart constructor.
newtype ChangeMessageVisibilityBatchResultEntry = ChangeMessageVisibilityBatchResultEntry'
  { _cmvbreId ::
      Text
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'ChangeMessageVisibilityBatchResultEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmvbreId' - Represents a message whose visibility timeout has been changed successfully.
changeMessageVisibilityBatchResultEntry ::
  -- | 'cmvbreId'
  Text ->
  ChangeMessageVisibilityBatchResultEntry
changeMessageVisibilityBatchResultEntry pId_ =
  ChangeMessageVisibilityBatchResultEntry' {_cmvbreId = pId_}

-- | Represents a message whose visibility timeout has been changed successfully.
cmvbreId :: Lens' ChangeMessageVisibilityBatchResultEntry Text
cmvbreId = lens _cmvbreId (\s a -> s {_cmvbreId = a})

instance FromXML ChangeMessageVisibilityBatchResultEntry where
  parseXML x =
    ChangeMessageVisibilityBatchResultEntry' <$> (x .@ "Id")

instance Hashable ChangeMessageVisibilityBatchResultEntry

instance NFData ChangeMessageVisibilityBatchResultEntry
