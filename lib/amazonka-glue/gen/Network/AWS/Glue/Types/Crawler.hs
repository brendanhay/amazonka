{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Crawler
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Crawler where

import Network.AWS.Glue.Types.CrawlerState
import Network.AWS.Glue.Types.CrawlerTargets
import Network.AWS.Glue.Types.LastCrawlInfo
import Network.AWS.Glue.Types.LineageConfiguration
import Network.AWS.Glue.Types.RecrawlPolicy
import Network.AWS.Glue.Types.Schedule
import Network.AWS.Glue.Types.SchemaChangePolicy
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies a crawler program that examines a data source and uses classifiers to try to determine its schema. If successful, the crawler records metadata concerning the data source in the AWS Glue Data Catalog.
--
--
--
-- /See:/ 'crawler' smart constructor.
data Crawler = Crawler'
  { _ccCreationTime :: !(Maybe POSIX),
    _ccState :: !(Maybe CrawlerState),
    _ccSchemaChangePolicy :: !(Maybe SchemaChangePolicy),
    _ccLastUpdated :: !(Maybe POSIX),
    _ccSchedule :: !(Maybe Schedule),
    _ccLastCrawl :: !(Maybe LastCrawlInfo),
    _ccCrawlElapsedTime :: !(Maybe Integer),
    _ccRecrawlPolicy :: !(Maybe RecrawlPolicy),
    _ccClassifiers :: !(Maybe [Text]),
    _ccRole :: !(Maybe Text),
    _ccName :: !(Maybe Text),
    _ccTargets :: !(Maybe CrawlerTargets),
    _ccVersion :: !(Maybe Integer),
    _ccDatabaseName :: !(Maybe Text),
    _ccCrawlerSecurityConfiguration :: !(Maybe Text),
    _ccLineageConfiguration :: !(Maybe LineageConfiguration),
    _ccConfiguration :: !(Maybe Text),
    _ccTablePrefix :: !(Maybe Text),
    _ccDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Crawler' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccCreationTime' - The time that the crawler was created.
--
-- * 'ccState' - Indicates whether the crawler is running, or whether a run is pending.
--
-- * 'ccSchemaChangePolicy' - The policy that specifies update and delete behaviors for the crawler.
--
-- * 'ccLastUpdated' - The time that the crawler was last updated.
--
-- * 'ccSchedule' - For scheduled crawlers, the schedule when the crawler runs.
--
-- * 'ccLastCrawl' - The status of the last crawl, and potentially error information if an error occurred.
--
-- * 'ccCrawlElapsedTime' - If the crawler is running, contains the total time elapsed since the last crawl began.
--
-- * 'ccRecrawlPolicy' - A policy that specifies whether to crawl the entire dataset again, or to crawl only folders that were added since the last crawler run.
--
-- * 'ccClassifiers' - A list of UTF-8 strings that specify the custom classifiers that are associated with the crawler.
--
-- * 'ccRole' - The Amazon Resource Name (ARN) of an IAM role that's used to access customer resources, such as Amazon Simple Storage Service (Amazon S3) data.
--
-- * 'ccName' - The name of the crawler.
--
-- * 'ccTargets' - A collection of targets to crawl.
--
-- * 'ccVersion' - The version of the crawler.
--
-- * 'ccDatabaseName' - The name of the database in which the crawler's output is stored.
--
-- * 'ccCrawlerSecurityConfiguration' - The name of the @SecurityConfiguration@ structure to be used by this crawler.
--
-- * 'ccLineageConfiguration' - A configuration that specifies whether data lineage is enabled for the crawler.
--
-- * 'ccConfiguration' - Crawler configuration information. This versioned JSON string allows users to specify aspects of a crawler's behavior. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/crawler-configuration.html Configuring a Crawler> .
--
-- * 'ccTablePrefix' - The prefix added to the names of tables that are created.
--
-- * 'ccDescription' - A description of the crawler.
crawler ::
  Crawler
crawler =
  Crawler'
    { _ccCreationTime = Nothing,
      _ccState = Nothing,
      _ccSchemaChangePolicy = Nothing,
      _ccLastUpdated = Nothing,
      _ccSchedule = Nothing,
      _ccLastCrawl = Nothing,
      _ccCrawlElapsedTime = Nothing,
      _ccRecrawlPolicy = Nothing,
      _ccClassifiers = Nothing,
      _ccRole = Nothing,
      _ccName = Nothing,
      _ccTargets = Nothing,
      _ccVersion = Nothing,
      _ccDatabaseName = Nothing,
      _ccCrawlerSecurityConfiguration = Nothing,
      _ccLineageConfiguration = Nothing,
      _ccConfiguration = Nothing,
      _ccTablePrefix = Nothing,
      _ccDescription = Nothing
    }

-- | The time that the crawler was created.
ccCreationTime :: Lens' Crawler (Maybe UTCTime)
ccCreationTime = lens _ccCreationTime (\s a -> s {_ccCreationTime = a}) . mapping _Time

-- | Indicates whether the crawler is running, or whether a run is pending.
ccState :: Lens' Crawler (Maybe CrawlerState)
ccState = lens _ccState (\s a -> s {_ccState = a})

-- | The policy that specifies update and delete behaviors for the crawler.
ccSchemaChangePolicy :: Lens' Crawler (Maybe SchemaChangePolicy)
ccSchemaChangePolicy = lens _ccSchemaChangePolicy (\s a -> s {_ccSchemaChangePolicy = a})

-- | The time that the crawler was last updated.
ccLastUpdated :: Lens' Crawler (Maybe UTCTime)
ccLastUpdated = lens _ccLastUpdated (\s a -> s {_ccLastUpdated = a}) . mapping _Time

-- | For scheduled crawlers, the schedule when the crawler runs.
ccSchedule :: Lens' Crawler (Maybe Schedule)
ccSchedule = lens _ccSchedule (\s a -> s {_ccSchedule = a})

-- | The status of the last crawl, and potentially error information if an error occurred.
ccLastCrawl :: Lens' Crawler (Maybe LastCrawlInfo)
ccLastCrawl = lens _ccLastCrawl (\s a -> s {_ccLastCrawl = a})

-- | If the crawler is running, contains the total time elapsed since the last crawl began.
ccCrawlElapsedTime :: Lens' Crawler (Maybe Integer)
ccCrawlElapsedTime = lens _ccCrawlElapsedTime (\s a -> s {_ccCrawlElapsedTime = a})

-- | A policy that specifies whether to crawl the entire dataset again, or to crawl only folders that were added since the last crawler run.
ccRecrawlPolicy :: Lens' Crawler (Maybe RecrawlPolicy)
ccRecrawlPolicy = lens _ccRecrawlPolicy (\s a -> s {_ccRecrawlPolicy = a})

-- | A list of UTF-8 strings that specify the custom classifiers that are associated with the crawler.
ccClassifiers :: Lens' Crawler [Text]
ccClassifiers = lens _ccClassifiers (\s a -> s {_ccClassifiers = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of an IAM role that's used to access customer resources, such as Amazon Simple Storage Service (Amazon S3) data.
ccRole :: Lens' Crawler (Maybe Text)
ccRole = lens _ccRole (\s a -> s {_ccRole = a})

-- | The name of the crawler.
ccName :: Lens' Crawler (Maybe Text)
ccName = lens _ccName (\s a -> s {_ccName = a})

-- | A collection of targets to crawl.
ccTargets :: Lens' Crawler (Maybe CrawlerTargets)
ccTargets = lens _ccTargets (\s a -> s {_ccTargets = a})

-- | The version of the crawler.
ccVersion :: Lens' Crawler (Maybe Integer)
ccVersion = lens _ccVersion (\s a -> s {_ccVersion = a})

-- | The name of the database in which the crawler's output is stored.
ccDatabaseName :: Lens' Crawler (Maybe Text)
ccDatabaseName = lens _ccDatabaseName (\s a -> s {_ccDatabaseName = a})

-- | The name of the @SecurityConfiguration@ structure to be used by this crawler.
ccCrawlerSecurityConfiguration :: Lens' Crawler (Maybe Text)
ccCrawlerSecurityConfiguration = lens _ccCrawlerSecurityConfiguration (\s a -> s {_ccCrawlerSecurityConfiguration = a})

-- | A configuration that specifies whether data lineage is enabled for the crawler.
ccLineageConfiguration :: Lens' Crawler (Maybe LineageConfiguration)
ccLineageConfiguration = lens _ccLineageConfiguration (\s a -> s {_ccLineageConfiguration = a})

-- | Crawler configuration information. This versioned JSON string allows users to specify aspects of a crawler's behavior. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/crawler-configuration.html Configuring a Crawler> .
ccConfiguration :: Lens' Crawler (Maybe Text)
ccConfiguration = lens _ccConfiguration (\s a -> s {_ccConfiguration = a})

-- | The prefix added to the names of tables that are created.
ccTablePrefix :: Lens' Crawler (Maybe Text)
ccTablePrefix = lens _ccTablePrefix (\s a -> s {_ccTablePrefix = a})

-- | A description of the crawler.
ccDescription :: Lens' Crawler (Maybe Text)
ccDescription = lens _ccDescription (\s a -> s {_ccDescription = a})

instance FromJSON Crawler where
  parseJSON =
    withObject
      "Crawler"
      ( \x ->
          Crawler'
            <$> (x .:? "CreationTime")
            <*> (x .:? "State")
            <*> (x .:? "SchemaChangePolicy")
            <*> (x .:? "LastUpdated")
            <*> (x .:? "Schedule")
            <*> (x .:? "LastCrawl")
            <*> (x .:? "CrawlElapsedTime")
            <*> (x .:? "RecrawlPolicy")
            <*> (x .:? "Classifiers" .!= mempty)
            <*> (x .:? "Role")
            <*> (x .:? "Name")
            <*> (x .:? "Targets")
            <*> (x .:? "Version")
            <*> (x .:? "DatabaseName")
            <*> (x .:? "CrawlerSecurityConfiguration")
            <*> (x .:? "LineageConfiguration")
            <*> (x .:? "Configuration")
            <*> (x .:? "TablePrefix")
            <*> (x .:? "Description")
      )

instance Hashable Crawler

instance NFData Crawler
