{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.CreateCrawler
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new crawler with specified targets, role, configuration, and optional schedule. At least one crawl target must be specified, in the @s3Targets@ field, the @jdbcTargets@ field, or the @DynamoDBTargets@ field.
module Network.AWS.Glue.CreateCrawler
  ( -- * Creating a Request
    createCrawler,
    CreateCrawler,

    -- * Request Lenses
    creSchemaChangePolicy,
    creSchedule,
    creRecrawlPolicy,
    creClassifiers,
    creDatabaseName,
    creCrawlerSecurityConfiguration,
    creLineageConfiguration,
    creConfiguration,
    creTablePrefix,
    creDescription,
    creTags,
    creName,
    creRole,
    creTargets,

    -- * Destructuring the Response
    createCrawlerResponse,
    CreateCrawlerResponse,

    -- * Response Lenses
    cccrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createCrawler' smart constructor.
data CreateCrawler = CreateCrawler'
  { _creSchemaChangePolicy ::
      !(Maybe SchemaChangePolicy),
    _creSchedule :: !(Maybe Text),
    _creRecrawlPolicy :: !(Maybe RecrawlPolicy),
    _creClassifiers :: !(Maybe [Text]),
    _creDatabaseName :: !(Maybe Text),
    _creCrawlerSecurityConfiguration :: !(Maybe Text),
    _creLineageConfiguration :: !(Maybe LineageConfiguration),
    _creConfiguration :: !(Maybe Text),
    _creTablePrefix :: !(Maybe Text),
    _creDescription :: !(Maybe Text),
    _creTags :: !(Maybe (Map Text (Text))),
    _creName :: !Text,
    _creRole :: !Text,
    _creTargets :: !CrawlerTargets
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateCrawler' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'creSchemaChangePolicy' - The policy for the crawler's update and deletion behavior.
--
-- * 'creSchedule' - A @cron@ expression used to specify the schedule (see <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ .
--
-- * 'creRecrawlPolicy' - A policy that specifies whether to crawl the entire dataset again, or to crawl only folders that were added since the last crawler run.
--
-- * 'creClassifiers' - A list of custom classifiers that the user has registered. By default, all built-in classifiers are included in a crawl, but these custom classifiers always override the default classifiers for a given classification.
--
-- * 'creDatabaseName' - The AWS Glue database where results are written, such as: @arn:aws:daylight:us-east-1::database/sometable/*@ .
--
-- * 'creCrawlerSecurityConfiguration' - The name of the @SecurityConfiguration@ structure to be used by this crawler.
--
-- * 'creLineageConfiguration' - Specifies data lineage configuration settings for the crawler.
--
-- * 'creConfiguration' - Crawler configuration information. This versioned JSON string allows users to specify aspects of a crawler's behavior. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/crawler-configuration.html Configuring a Crawler> .
--
-- * 'creTablePrefix' - The table prefix used for catalog tables that are created.
--
-- * 'creDescription' - A description of the new crawler.
--
-- * 'creTags' - The tags to use with this crawler request. You may use tags to limit access to the crawler. For more information about tags in AWS Glue, see <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html AWS Tags in AWS Glue> in the developer guide.
--
-- * 'creName' - Name of the new crawler.
--
-- * 'creRole' - The IAM role or Amazon Resource Name (ARN) of an IAM role used by the new crawler to access customer resources.
--
-- * 'creTargets' - A list of collection of targets to crawl.
createCrawler ::
  -- | 'creName'
  Text ->
  -- | 'creRole'
  Text ->
  -- | 'creTargets'
  CrawlerTargets ->
  CreateCrawler
createCrawler pName_ pRole_ pTargets_ =
  CreateCrawler'
    { _creSchemaChangePolicy = Nothing,
      _creSchedule = Nothing,
      _creRecrawlPolicy = Nothing,
      _creClassifiers = Nothing,
      _creDatabaseName = Nothing,
      _creCrawlerSecurityConfiguration = Nothing,
      _creLineageConfiguration = Nothing,
      _creConfiguration = Nothing,
      _creTablePrefix = Nothing,
      _creDescription = Nothing,
      _creTags = Nothing,
      _creName = pName_,
      _creRole = pRole_,
      _creTargets = pTargets_
    }

-- | The policy for the crawler's update and deletion behavior.
creSchemaChangePolicy :: Lens' CreateCrawler (Maybe SchemaChangePolicy)
creSchemaChangePolicy = lens _creSchemaChangePolicy (\s a -> s {_creSchemaChangePolicy = a})

-- | A @cron@ expression used to specify the schedule (see <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ .
creSchedule :: Lens' CreateCrawler (Maybe Text)
creSchedule = lens _creSchedule (\s a -> s {_creSchedule = a})

-- | A policy that specifies whether to crawl the entire dataset again, or to crawl only folders that were added since the last crawler run.
creRecrawlPolicy :: Lens' CreateCrawler (Maybe RecrawlPolicy)
creRecrawlPolicy = lens _creRecrawlPolicy (\s a -> s {_creRecrawlPolicy = a})

-- | A list of custom classifiers that the user has registered. By default, all built-in classifiers are included in a crawl, but these custom classifiers always override the default classifiers for a given classification.
creClassifiers :: Lens' CreateCrawler [Text]
creClassifiers = lens _creClassifiers (\s a -> s {_creClassifiers = a}) . _Default . _Coerce

-- | The AWS Glue database where results are written, such as: @arn:aws:daylight:us-east-1::database/sometable/*@ .
creDatabaseName :: Lens' CreateCrawler (Maybe Text)
creDatabaseName = lens _creDatabaseName (\s a -> s {_creDatabaseName = a})

-- | The name of the @SecurityConfiguration@ structure to be used by this crawler.
creCrawlerSecurityConfiguration :: Lens' CreateCrawler (Maybe Text)
creCrawlerSecurityConfiguration = lens _creCrawlerSecurityConfiguration (\s a -> s {_creCrawlerSecurityConfiguration = a})

-- | Specifies data lineage configuration settings for the crawler.
creLineageConfiguration :: Lens' CreateCrawler (Maybe LineageConfiguration)
creLineageConfiguration = lens _creLineageConfiguration (\s a -> s {_creLineageConfiguration = a})

-- | Crawler configuration information. This versioned JSON string allows users to specify aspects of a crawler's behavior. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/crawler-configuration.html Configuring a Crawler> .
creConfiguration :: Lens' CreateCrawler (Maybe Text)
creConfiguration = lens _creConfiguration (\s a -> s {_creConfiguration = a})

-- | The table prefix used for catalog tables that are created.
creTablePrefix :: Lens' CreateCrawler (Maybe Text)
creTablePrefix = lens _creTablePrefix (\s a -> s {_creTablePrefix = a})

-- | A description of the new crawler.
creDescription :: Lens' CreateCrawler (Maybe Text)
creDescription = lens _creDescription (\s a -> s {_creDescription = a})

-- | The tags to use with this crawler request. You may use tags to limit access to the crawler. For more information about tags in AWS Glue, see <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html AWS Tags in AWS Glue> in the developer guide.
creTags :: Lens' CreateCrawler (HashMap Text (Text))
creTags = lens _creTags (\s a -> s {_creTags = a}) . _Default . _Map

-- | Name of the new crawler.
creName :: Lens' CreateCrawler Text
creName = lens _creName (\s a -> s {_creName = a})

-- | The IAM role or Amazon Resource Name (ARN) of an IAM role used by the new crawler to access customer resources.
creRole :: Lens' CreateCrawler Text
creRole = lens _creRole (\s a -> s {_creRole = a})

-- | A list of collection of targets to crawl.
creTargets :: Lens' CreateCrawler CrawlerTargets
creTargets = lens _creTargets (\s a -> s {_creTargets = a})

instance AWSRequest CreateCrawler where
  type Rs CreateCrawler = CreateCrawlerResponse
  request = postJSON glue
  response =
    receiveEmpty
      (\s h x -> CreateCrawlerResponse' <$> (pure (fromEnum s)))

instance Hashable CreateCrawler

instance NFData CreateCrawler

instance ToHeaders CreateCrawler where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSGlue.CreateCrawler" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateCrawler where
  toJSON CreateCrawler' {..} =
    object
      ( catMaybes
          [ ("SchemaChangePolicy" .=) <$> _creSchemaChangePolicy,
            ("Schedule" .=) <$> _creSchedule,
            ("RecrawlPolicy" .=) <$> _creRecrawlPolicy,
            ("Classifiers" .=) <$> _creClassifiers,
            ("DatabaseName" .=) <$> _creDatabaseName,
            ("CrawlerSecurityConfiguration" .=)
              <$> _creCrawlerSecurityConfiguration,
            ("LineageConfiguration" .=) <$> _creLineageConfiguration,
            ("Configuration" .=) <$> _creConfiguration,
            ("TablePrefix" .=) <$> _creTablePrefix,
            ("Description" .=) <$> _creDescription,
            ("Tags" .=) <$> _creTags,
            Just ("Name" .= _creName),
            Just ("Role" .= _creRole),
            Just ("Targets" .= _creTargets)
          ]
      )

instance ToPath CreateCrawler where
  toPath = const "/"

instance ToQuery CreateCrawler where
  toQuery = const mempty

-- | /See:/ 'createCrawlerResponse' smart constructor.
newtype CreateCrawlerResponse = CreateCrawlerResponse'
  { _cccrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateCrawlerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cccrsResponseStatus' - -- | The response status code.
createCrawlerResponse ::
  -- | 'cccrsResponseStatus'
  Int ->
  CreateCrawlerResponse
createCrawlerResponse pResponseStatus_ =
  CreateCrawlerResponse' {_cccrsResponseStatus = pResponseStatus_}

-- | -- | The response status code.
cccrsResponseStatus :: Lens' CreateCrawlerResponse Int
cccrsResponseStatus = lens _cccrsResponseStatus (\s a -> s {_cccrsResponseStatus = a})

instance NFData CreateCrawlerResponse
