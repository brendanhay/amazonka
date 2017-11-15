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
-- Module      : Network.AWS.Glue.CreateCrawler
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new @Crawler@ with specified targets, role, configuration, and optional schedule. At least one crawl target must be specified, in either the /s3Targets/ or the /jdbcTargets/ field.
--
--
module Network.AWS.Glue.CreateCrawler
    (
    -- * Creating a Request
      createCrawler
    , CreateCrawler
    -- * Request Lenses
    , ccSchemaChangePolicy
    , ccSchedule
    , ccClassifiers
    , ccTablePrefix
    , ccDescription
    , ccName
    , ccRole
    , ccDatabaseName
    , ccTargets

    -- * Destructuring the Response
    , createCrawlerResponse
    , CreateCrawlerResponse
    -- * Response Lenses
    , crersResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createCrawler' smart constructor.
data CreateCrawler = CreateCrawler'
  { _ccSchemaChangePolicy :: !(Maybe SchemaChangePolicy)
  , _ccSchedule           :: !(Maybe Text)
  , _ccClassifiers        :: !(Maybe [Text])
  , _ccTablePrefix        :: !(Maybe Text)
  , _ccDescription        :: !(Maybe Text)
  , _ccName               :: !Text
  , _ccRole               :: !Text
  , _ccDatabaseName       :: !Text
  , _ccTargets            :: !CrawlerTargets
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateCrawler' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccSchemaChangePolicy' - Policy for the crawler's update and deletion behavior.
--
-- * 'ccSchedule' - A @cron@ expression used to specify the schedule (see <http://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ .
--
-- * 'ccClassifiers' - A list of custom @Classifier@ names that the user has registered. By default, all AWS classifiers are included in a crawl, but these custom classifiers always override the default classifiers for a given classification.
--
-- * 'ccTablePrefix' - The table prefix used for catalog tables created.
--
-- * 'ccDescription' - A description of the new @Crawler@ .
--
-- * 'ccName' - Name of the new @Crawler@ .
--
-- * 'ccRole' - The IAM role (or ARN of an IAM role) used by the new @Crawler@ to access customer resources.
--
-- * 'ccDatabaseName' - The Glue @Database@ where results will be stored, such as: @arn:aws:daylight:us-east-1::database/sometable/*@ .
--
-- * 'ccTargets' - A list of collection of targets to crawl.
createCrawler
    :: Text -- ^ 'ccName'
    -> Text -- ^ 'ccRole'
    -> Text -- ^ 'ccDatabaseName'
    -> CrawlerTargets -- ^ 'ccTargets'
    -> CreateCrawler
createCrawler pName_ pRole_ pDatabaseName_ pTargets_ =
  CreateCrawler'
  { _ccSchemaChangePolicy = Nothing
  , _ccSchedule = Nothing
  , _ccClassifiers = Nothing
  , _ccTablePrefix = Nothing
  , _ccDescription = Nothing
  , _ccName = pName_
  , _ccRole = pRole_
  , _ccDatabaseName = pDatabaseName_
  , _ccTargets = pTargets_
  }


-- | Policy for the crawler's update and deletion behavior.
ccSchemaChangePolicy :: Lens' CreateCrawler (Maybe SchemaChangePolicy)
ccSchemaChangePolicy = lens _ccSchemaChangePolicy (\ s a -> s{_ccSchemaChangePolicy = a});

-- | A @cron@ expression used to specify the schedule (see <http://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ .
ccSchedule :: Lens' CreateCrawler (Maybe Text)
ccSchedule = lens _ccSchedule (\ s a -> s{_ccSchedule = a});

-- | A list of custom @Classifier@ names that the user has registered. By default, all AWS classifiers are included in a crawl, but these custom classifiers always override the default classifiers for a given classification.
ccClassifiers :: Lens' CreateCrawler [Text]
ccClassifiers = lens _ccClassifiers (\ s a -> s{_ccClassifiers = a}) . _Default . _Coerce;

-- | The table prefix used for catalog tables created.
ccTablePrefix :: Lens' CreateCrawler (Maybe Text)
ccTablePrefix = lens _ccTablePrefix (\ s a -> s{_ccTablePrefix = a});

-- | A description of the new @Crawler@ .
ccDescription :: Lens' CreateCrawler (Maybe Text)
ccDescription = lens _ccDescription (\ s a -> s{_ccDescription = a});

-- | Name of the new @Crawler@ .
ccName :: Lens' CreateCrawler Text
ccName = lens _ccName (\ s a -> s{_ccName = a});

-- | The IAM role (or ARN of an IAM role) used by the new @Crawler@ to access customer resources.
ccRole :: Lens' CreateCrawler Text
ccRole = lens _ccRole (\ s a -> s{_ccRole = a});

-- | The Glue @Database@ where results will be stored, such as: @arn:aws:daylight:us-east-1::database/sometable/*@ .
ccDatabaseName :: Lens' CreateCrawler Text
ccDatabaseName = lens _ccDatabaseName (\ s a -> s{_ccDatabaseName = a});

-- | A list of collection of targets to crawl.
ccTargets :: Lens' CreateCrawler CrawlerTargets
ccTargets = lens _ccTargets (\ s a -> s{_ccTargets = a});

instance AWSRequest CreateCrawler where
        type Rs CreateCrawler = CreateCrawlerResponse
        request = postJSON glue
        response
          = receiveEmpty
              (\ s h x ->
                 CreateCrawlerResponse' <$> (pure (fromEnum s)))

instance Hashable CreateCrawler where

instance NFData CreateCrawler where

instance ToHeaders CreateCrawler where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.CreateCrawler" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateCrawler where
        toJSON CreateCrawler'{..}
          = object
              (catMaybes
                 [("SchemaChangePolicy" .=) <$> _ccSchemaChangePolicy,
                  ("Schedule" .=) <$> _ccSchedule,
                  ("Classifiers" .=) <$> _ccClassifiers,
                  ("TablePrefix" .=) <$> _ccTablePrefix,
                  ("Description" .=) <$> _ccDescription,
                  Just ("Name" .= _ccName), Just ("Role" .= _ccRole),
                  Just ("DatabaseName" .= _ccDatabaseName),
                  Just ("Targets" .= _ccTargets)])

instance ToPath CreateCrawler where
        toPath = const "/"

instance ToQuery CreateCrawler where
        toQuery = const mempty

-- | /See:/ 'createCrawlerResponse' smart constructor.
newtype CreateCrawlerResponse = CreateCrawlerResponse'
  { _crersResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateCrawlerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crersResponseStatus' - -- | The response status code.
createCrawlerResponse
    :: Int -- ^ 'crersResponseStatus'
    -> CreateCrawlerResponse
createCrawlerResponse pResponseStatus_ =
  CreateCrawlerResponse' {_crersResponseStatus = pResponseStatus_}


-- | -- | The response status code.
crersResponseStatus :: Lens' CreateCrawlerResponse Int
crersResponseStatus = lens _crersResponseStatus (\ s a -> s{_crersResponseStatus = a});

instance NFData CreateCrawlerResponse where
