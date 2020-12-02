{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ExportSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ExportSummary where

import Network.AWS.DynamoDB.Types.ExportStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Summary information about an export task.
--
--
--
-- /See:/ 'exportSummary' smart constructor.
data ExportSummary = ExportSummary'
  { _esExportStatus ::
      !(Maybe ExportStatus),
    _esExportARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExportSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esExportStatus' - Export can be in one of the following states: IN_PROGRESS, COMPLETED, or FAILED.
--
-- * 'esExportARN' - The Amazon Resource Name (ARN) of the export.
exportSummary ::
  ExportSummary
exportSummary =
  ExportSummary' {_esExportStatus = Nothing, _esExportARN = Nothing}

-- | Export can be in one of the following states: IN_PROGRESS, COMPLETED, or FAILED.
esExportStatus :: Lens' ExportSummary (Maybe ExportStatus)
esExportStatus = lens _esExportStatus (\s a -> s {_esExportStatus = a})

-- | The Amazon Resource Name (ARN) of the export.
esExportARN :: Lens' ExportSummary (Maybe Text)
esExportARN = lens _esExportARN (\s a -> s {_esExportARN = a})

instance FromJSON ExportSummary where
  parseJSON =
    withObject
      "ExportSummary"
      ( \x ->
          ExportSummary' <$> (x .:? "ExportStatus") <*> (x .:? "ExportArn")
      )

instance Hashable ExportSummary

instance NFData ExportSummary
