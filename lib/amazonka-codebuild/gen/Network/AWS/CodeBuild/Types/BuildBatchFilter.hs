{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.BuildBatchFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.BuildBatchFilter where

import Network.AWS.CodeBuild.Types.StatusType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies filters when retrieving batch builds.
--
--
--
-- /See:/ 'buildBatchFilter' smart constructor.
newtype BuildBatchFilter = BuildBatchFilter'
  { _bbfStatus ::
      Maybe StatusType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BuildBatchFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bbfStatus' - The status of the batch builds to retrieve. Only batch builds that have this status will be retrieved.
buildBatchFilter ::
  BuildBatchFilter
buildBatchFilter = BuildBatchFilter' {_bbfStatus = Nothing}

-- | The status of the batch builds to retrieve. Only batch builds that have this status will be retrieved.
bbfStatus :: Lens' BuildBatchFilter (Maybe StatusType)
bbfStatus = lens _bbfStatus (\s a -> s {_bbfStatus = a})

instance Hashable BuildBatchFilter

instance NFData BuildBatchFilter

instance ToJSON BuildBatchFilter where
  toJSON BuildBatchFilter' {..} =
    object (catMaybes [("status" .=) <$> _bbfStatus])
