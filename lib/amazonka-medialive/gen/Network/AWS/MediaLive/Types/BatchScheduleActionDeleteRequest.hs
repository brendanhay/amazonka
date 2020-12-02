{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.BatchScheduleActionDeleteRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.BatchScheduleActionDeleteRequest where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A list of schedule actions to delete.
--
-- /See:/ 'batchScheduleActionDeleteRequest' smart constructor.
newtype BatchScheduleActionDeleteRequest = BatchScheduleActionDeleteRequest'
  { _bsadrActionNames ::
      [Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchScheduleActionDeleteRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bsadrActionNames' - A list of schedule actions to delete.
batchScheduleActionDeleteRequest ::
  BatchScheduleActionDeleteRequest
batchScheduleActionDeleteRequest =
  BatchScheduleActionDeleteRequest' {_bsadrActionNames = mempty}

-- | A list of schedule actions to delete.
bsadrActionNames :: Lens' BatchScheduleActionDeleteRequest [Text]
bsadrActionNames = lens _bsadrActionNames (\s a -> s {_bsadrActionNames = a}) . _Coerce

instance Hashable BatchScheduleActionDeleteRequest

instance NFData BatchScheduleActionDeleteRequest

instance ToJSON BatchScheduleActionDeleteRequest where
  toJSON BatchScheduleActionDeleteRequest' {..} =
    object (catMaybes [Just ("actionNames" .= _bsadrActionNames)])
