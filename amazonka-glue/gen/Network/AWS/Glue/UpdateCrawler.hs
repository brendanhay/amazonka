{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.UpdateCrawler
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a crawler. If a crawler is running, you must stop it using @StopCrawler@ before updating it.
--
--
module Network.AWS.Glue.UpdateCrawler
    (
    -- * Creating a Request
      updateCrawler
    , UpdateCrawler
    -- * Request Lenses
    , uSchemaChangePolicy
    , uSchedule
    , uClassifiers
    , uRole
    , uTargets
    , uDatabaseName
    , uConfiguration
    , uTablePrefix
    , uDescription
    , uName

    -- * Destructuring the Response
    , updateCrawlerResponse
    , UpdateCrawlerResponse
    -- * Response Lenses
    , uccrsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateCrawler' smart constructor.
data UpdateCrawler = UpdateCrawler'
  { _uSchemaChangePolicy :: !(Maybe SchemaChangePolicy)
  , _uSchedule           :: !(Maybe Text)
  , _uClassifiers        :: !(Maybe [Text])
  , _uRole               :: !(Maybe Text)
  , _uTargets            :: !(Maybe CrawlerTargets)
  , _uDatabaseName       :: !(Maybe Text)
  , _uConfiguration      :: !(Maybe Text)
  , _uTablePrefix        :: !(Maybe Text)
  , _uDescription        :: !(Maybe Text)
  , _uName               :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateCrawler' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uSchemaChangePolicy' - Policy for the crawler's update and deletion behavior.
--
-- * 'uSchedule' - A @cron@ expression used to specify the schedule (see <http://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ .
--
-- * 'uClassifiers' - A list of custom classifiers that the user has registered. By default, all classifiers are included in a crawl, but these custom classifiers always override the default classifiers for a given classification.
--
-- * 'uRole' - The IAM role (or ARN of an IAM role) used by the new crawler to access customer resources.
--
-- * 'uTargets' - A list of targets to crawl.
--
-- * 'uDatabaseName' - The AWS Glue database where results are stored, such as: @arn:aws:daylight:us-east-1::database/sometable/*@ .
--
-- * 'uConfiguration' - Crawler configuration information. This versioned JSON string allows users to specify aspects of a Crawler's behavior. You can use this field to force partitions to inherit metadata such as classification, input format, output format, serde information, and schema from their parent table, rather than detect this information separately for each partition. Use the following JSON string to specify that behavior: Example: @'{ "Version": 1.0, "CrawlerOutput": { "Partitions": { "AddOrUpdateBehavior": "InheritFromTable" } } }'@
--
-- * 'uTablePrefix' - The table prefix used for catalog tables that are created.
--
-- * 'uDescription' - A description of the new crawler.
--
-- * 'uName' - Name of the new crawler.
updateCrawler
    :: Text -- ^ 'uName'
    -> UpdateCrawler
updateCrawler pName_ =
  UpdateCrawler'
    { _uSchemaChangePolicy = Nothing
    , _uSchedule = Nothing
    , _uClassifiers = Nothing
    , _uRole = Nothing
    , _uTargets = Nothing
    , _uDatabaseName = Nothing
    , _uConfiguration = Nothing
    , _uTablePrefix = Nothing
    , _uDescription = Nothing
    , _uName = pName_
    }


-- | Policy for the crawler's update and deletion behavior.
uSchemaChangePolicy :: Lens' UpdateCrawler (Maybe SchemaChangePolicy)
uSchemaChangePolicy = lens _uSchemaChangePolicy (\ s a -> s{_uSchemaChangePolicy = a})

-- | A @cron@ expression used to specify the schedule (see <http://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ .
uSchedule :: Lens' UpdateCrawler (Maybe Text)
uSchedule = lens _uSchedule (\ s a -> s{_uSchedule = a})

-- | A list of custom classifiers that the user has registered. By default, all classifiers are included in a crawl, but these custom classifiers always override the default classifiers for a given classification.
uClassifiers :: Lens' UpdateCrawler [Text]
uClassifiers = lens _uClassifiers (\ s a -> s{_uClassifiers = a}) . _Default . _Coerce

-- | The IAM role (or ARN of an IAM role) used by the new crawler to access customer resources.
uRole :: Lens' UpdateCrawler (Maybe Text)
uRole = lens _uRole (\ s a -> s{_uRole = a})

-- | A list of targets to crawl.
uTargets :: Lens' UpdateCrawler (Maybe CrawlerTargets)
uTargets = lens _uTargets (\ s a -> s{_uTargets = a})

-- | The AWS Glue database where results are stored, such as: @arn:aws:daylight:us-east-1::database/sometable/*@ .
uDatabaseName :: Lens' UpdateCrawler (Maybe Text)
uDatabaseName = lens _uDatabaseName (\ s a -> s{_uDatabaseName = a})

-- | Crawler configuration information. This versioned JSON string allows users to specify aspects of a Crawler's behavior. You can use this field to force partitions to inherit metadata such as classification, input format, output format, serde information, and schema from their parent table, rather than detect this information separately for each partition. Use the following JSON string to specify that behavior: Example: @'{ "Version": 1.0, "CrawlerOutput": { "Partitions": { "AddOrUpdateBehavior": "InheritFromTable" } } }'@
uConfiguration :: Lens' UpdateCrawler (Maybe Text)
uConfiguration = lens _uConfiguration (\ s a -> s{_uConfiguration = a})

-- | The table prefix used for catalog tables that are created.
uTablePrefix :: Lens' UpdateCrawler (Maybe Text)
uTablePrefix = lens _uTablePrefix (\ s a -> s{_uTablePrefix = a})

-- | A description of the new crawler.
uDescription :: Lens' UpdateCrawler (Maybe Text)
uDescription = lens _uDescription (\ s a -> s{_uDescription = a})

-- | Name of the new crawler.
uName :: Lens' UpdateCrawler Text
uName = lens _uName (\ s a -> s{_uName = a})

instance AWSRequest UpdateCrawler where
        type Rs UpdateCrawler = UpdateCrawlerResponse
        request = postJSON glue
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateCrawlerResponse' <$> (pure (fromEnum s)))

instance Hashable UpdateCrawler where

instance NFData UpdateCrawler where

instance ToHeaders UpdateCrawler where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.UpdateCrawler" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateCrawler where
        toJSON UpdateCrawler'{..}
          = object
              (catMaybes
                 [("SchemaChangePolicy" .=) <$> _uSchemaChangePolicy,
                  ("Schedule" .=) <$> _uSchedule,
                  ("Classifiers" .=) <$> _uClassifiers,
                  ("Role" .=) <$> _uRole, ("Targets" .=) <$> _uTargets,
                  ("DatabaseName" .=) <$> _uDatabaseName,
                  ("Configuration" .=) <$> _uConfiguration,
                  ("TablePrefix" .=) <$> _uTablePrefix,
                  ("Description" .=) <$> _uDescription,
                  Just ("Name" .= _uName)])

instance ToPath UpdateCrawler where
        toPath = const "/"

instance ToQuery UpdateCrawler where
        toQuery = const mempty

-- | /See:/ 'updateCrawlerResponse' smart constructor.
newtype UpdateCrawlerResponse = UpdateCrawlerResponse'
  { _uccrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateCrawlerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uccrsResponseStatus' - -- | The response status code.
updateCrawlerResponse
    :: Int -- ^ 'uccrsResponseStatus'
    -> UpdateCrawlerResponse
updateCrawlerResponse pResponseStatus_ =
  UpdateCrawlerResponse' {_uccrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
uccrsResponseStatus :: Lens' UpdateCrawlerResponse Int
uccrsResponseStatus = lens _uccrsResponseStatus (\ s a -> s{_uccrsResponseStatus = a})

instance NFData UpdateCrawlerResponse where
