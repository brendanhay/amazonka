{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostAndUsageReport.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostAndUsageReport.Types.Product where

import Network.AWS.CostAndUsageReport.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The definition of AWS Cost and Usage Report. Customer can specify the report name, time unit, report format, compression format, S3 bucket and additional artifacts and schema elements in the definition.
--
-- /See:/ 'reportDefinition' smart constructor.
data ReportDefinition = ReportDefinition'
  { _rdAdditionalArtifacts      :: !(Maybe [AdditionalArtifact])
  , _rdReportName               :: !Text
  , _rdTimeUnit                 :: !TimeUnit
  , _rdFormat                   :: !ReportFormat
  , _rdCompression              :: !CompressionFormat
  , _rdAdditionalSchemaElements :: ![SchemaElement]
  , _rdS3Bucket                 :: !Text
  , _rdS3Prefix                 :: !Text
  , _rdS3Region                 :: !AWSRegion
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReportDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdAdditionalArtifacts' - Undocumented member.
--
-- * 'rdReportName' - Undocumented member.
--
-- * 'rdTimeUnit' - Undocumented member.
--
-- * 'rdFormat' - Undocumented member.
--
-- * 'rdCompression' - Undocumented member.
--
-- * 'rdAdditionalSchemaElements' - Undocumented member.
--
-- * 'rdS3Bucket' - Undocumented member.
--
-- * 'rdS3Prefix' - Undocumented member.
--
-- * 'rdS3Region' - Undocumented member.
reportDefinition
    :: Text -- ^ 'rdReportName'
    -> TimeUnit -- ^ 'rdTimeUnit'
    -> ReportFormat -- ^ 'rdFormat'
    -> CompressionFormat -- ^ 'rdCompression'
    -> Text -- ^ 'rdS3Bucket'
    -> Text -- ^ 'rdS3Prefix'
    -> AWSRegion -- ^ 'rdS3Region'
    -> ReportDefinition
reportDefinition pReportName_ pTimeUnit_ pFormat_ pCompression_ pS3Bucket_ pS3Prefix_ pS3Region_ =
  ReportDefinition'
    { _rdAdditionalArtifacts = Nothing
    , _rdReportName = pReportName_
    , _rdTimeUnit = pTimeUnit_
    , _rdFormat = pFormat_
    , _rdCompression = pCompression_
    , _rdAdditionalSchemaElements = mempty
    , _rdS3Bucket = pS3Bucket_
    , _rdS3Prefix = pS3Prefix_
    , _rdS3Region = pS3Region_
    }


-- | Undocumented member.
rdAdditionalArtifacts :: Lens' ReportDefinition [AdditionalArtifact]
rdAdditionalArtifacts = lens _rdAdditionalArtifacts (\ s a -> s{_rdAdditionalArtifacts = a}) . _Default . _Coerce

-- | Undocumented member.
rdReportName :: Lens' ReportDefinition Text
rdReportName = lens _rdReportName (\ s a -> s{_rdReportName = a})

-- | Undocumented member.
rdTimeUnit :: Lens' ReportDefinition TimeUnit
rdTimeUnit = lens _rdTimeUnit (\ s a -> s{_rdTimeUnit = a})

-- | Undocumented member.
rdFormat :: Lens' ReportDefinition ReportFormat
rdFormat = lens _rdFormat (\ s a -> s{_rdFormat = a})

-- | Undocumented member.
rdCompression :: Lens' ReportDefinition CompressionFormat
rdCompression = lens _rdCompression (\ s a -> s{_rdCompression = a})

-- | Undocumented member.
rdAdditionalSchemaElements :: Lens' ReportDefinition [SchemaElement]
rdAdditionalSchemaElements = lens _rdAdditionalSchemaElements (\ s a -> s{_rdAdditionalSchemaElements = a}) . _Coerce

-- | Undocumented member.
rdS3Bucket :: Lens' ReportDefinition Text
rdS3Bucket = lens _rdS3Bucket (\ s a -> s{_rdS3Bucket = a})

-- | Undocumented member.
rdS3Prefix :: Lens' ReportDefinition Text
rdS3Prefix = lens _rdS3Prefix (\ s a -> s{_rdS3Prefix = a})

-- | Undocumented member.
rdS3Region :: Lens' ReportDefinition AWSRegion
rdS3Region = lens _rdS3Region (\ s a -> s{_rdS3Region = a})

instance FromJSON ReportDefinition where
        parseJSON
          = withObject "ReportDefinition"
              (\ x ->
                 ReportDefinition' <$>
                   (x .:? "AdditionalArtifacts" .!= mempty) <*>
                     (x .: "ReportName")
                     <*> (x .: "TimeUnit")
                     <*> (x .: "Format")
                     <*> (x .: "Compression")
                     <*> (x .:? "AdditionalSchemaElements" .!= mempty)
                     <*> (x .: "S3Bucket")
                     <*> (x .: "S3Prefix")
                     <*> (x .: "S3Region"))

instance Hashable ReportDefinition where

instance NFData ReportDefinition where

instance ToJSON ReportDefinition where
        toJSON ReportDefinition'{..}
          = object
              (catMaybes
                 [("AdditionalArtifacts" .=) <$>
                    _rdAdditionalArtifacts,
                  Just ("ReportName" .= _rdReportName),
                  Just ("TimeUnit" .= _rdTimeUnit),
                  Just ("Format" .= _rdFormat),
                  Just ("Compression" .= _rdCompression),
                  Just
                    ("AdditionalSchemaElements" .=
                       _rdAdditionalSchemaElements),
                  Just ("S3Bucket" .= _rdS3Bucket),
                  Just ("S3Prefix" .= _rdS3Prefix),
                  Just ("S3Region" .= _rdS3Region)])
