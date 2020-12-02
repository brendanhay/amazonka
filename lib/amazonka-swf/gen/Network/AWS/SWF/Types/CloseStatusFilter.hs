{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.CloseStatusFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.CloseStatusFilter where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SWF.Types.CloseStatus

-- | Used to filter the closed workflow executions in visibility APIs by their close status.
--
--
--
-- /See:/ 'closeStatusFilter' smart constructor.
newtype CloseStatusFilter = CloseStatusFilter'
  { _csfStatus ::
      CloseStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CloseStatusFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csfStatus' - The close status that must match the close status of an execution for it to meet the criteria of this filter.
closeStatusFilter ::
  -- | 'csfStatus'
  CloseStatus ->
  CloseStatusFilter
closeStatusFilter pStatus_ =
  CloseStatusFilter' {_csfStatus = pStatus_}

-- | The close status that must match the close status of an execution for it to meet the criteria of this filter.
csfStatus :: Lens' CloseStatusFilter CloseStatus
csfStatus = lens _csfStatus (\s a -> s {_csfStatus = a})

instance Hashable CloseStatusFilter

instance NFData CloseStatusFilter

instance ToJSON CloseStatusFilter where
  toJSON CloseStatusFilter' {..} =
    object (catMaybes [Just ("status" .= _csfStatus)])
