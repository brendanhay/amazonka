{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.StorageClassAnalysisDataExport
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.StorageClassAnalysisDataExport where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.AnalyticsExportDestination
import Network.AWS.S3.Types.StorageClassAnalysisSchemaVersion

-- | Container for data related to the storage class analysis for an Amazon S3 bucket for export.
--
--
--
-- /See:/ 'storageClassAnalysisDataExport' smart constructor.
data StorageClassAnalysisDataExport = StorageClassAnalysisDataExport'
  { _scadeOutputSchemaVersion ::
      !StorageClassAnalysisSchemaVersion,
    _scadeDestination ::
      !AnalyticsExportDestination
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StorageClassAnalysisDataExport' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scadeOutputSchemaVersion' - The version of the output schema to use when exporting data. Must be @V_1@ .
--
-- * 'scadeDestination' - The place to store the data for an analysis.
storageClassAnalysisDataExport ::
  -- | 'scadeOutputSchemaVersion'
  StorageClassAnalysisSchemaVersion ->
  -- | 'scadeDestination'
  AnalyticsExportDestination ->
  StorageClassAnalysisDataExport
storageClassAnalysisDataExport pOutputSchemaVersion_ pDestination_ =
  StorageClassAnalysisDataExport'
    { _scadeOutputSchemaVersion =
        pOutputSchemaVersion_,
      _scadeDestination = pDestination_
    }

-- | The version of the output schema to use when exporting data. Must be @V_1@ .
scadeOutputSchemaVersion :: Lens' StorageClassAnalysisDataExport StorageClassAnalysisSchemaVersion
scadeOutputSchemaVersion = lens _scadeOutputSchemaVersion (\s a -> s {_scadeOutputSchemaVersion = a})

-- | The place to store the data for an analysis.
scadeDestination :: Lens' StorageClassAnalysisDataExport AnalyticsExportDestination
scadeDestination = lens _scadeDestination (\s a -> s {_scadeDestination = a})

instance FromXML StorageClassAnalysisDataExport where
  parseXML x =
    StorageClassAnalysisDataExport'
      <$> (x .@ "OutputSchemaVersion") <*> (x .@ "Destination")

instance Hashable StorageClassAnalysisDataExport

instance NFData StorageClassAnalysisDataExport

instance ToXML StorageClassAnalysisDataExport where
  toXML StorageClassAnalysisDataExport' {..} =
    mconcat
      [ "OutputSchemaVersion" @= _scadeOutputSchemaVersion,
        "Destination" @= _scadeDestination
      ]
