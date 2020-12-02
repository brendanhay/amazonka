{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ReportFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ReportFilter where

import Network.AWS.CodeBuild.Types.ReportStatusType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A filter used to return reports with the status specified by the input @status@ parameter.
--
--
--
-- /See:/ 'reportFilter' smart constructor.
newtype ReportFilter = ReportFilter'
  { _rfStatus ::
      Maybe ReportStatusType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReportFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rfStatus' - The status used to filter reports. You can filter using one status only.
reportFilter ::
  ReportFilter
reportFilter = ReportFilter' {_rfStatus = Nothing}

-- | The status used to filter reports. You can filter using one status only.
rfStatus :: Lens' ReportFilter (Maybe ReportStatusType)
rfStatus = lens _rfStatus (\s a -> s {_rfStatus = a})

instance Hashable ReportFilter

instance NFData ReportFilter

instance ToJSON ReportFilter where
  toJSON ReportFilter' {..} =
    object (catMaybes [("status" .=) <$> _rfStatus])
