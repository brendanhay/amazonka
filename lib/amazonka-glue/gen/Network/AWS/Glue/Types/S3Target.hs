{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.S3Target
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.S3Target where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies a data store in Amazon Simple Storage Service (Amazon S3).
--
--
--
-- /See:/ 's3Target' smart constructor.
data S3Target = S3Target'
  { _stPath :: !(Maybe Text),
    _stConnectionName :: !(Maybe Text),
    _stExclusions :: !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'S3Target' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stPath' - The path to the Amazon S3 target.
--
-- * 'stConnectionName' - The name of a connection which allows a job or crawler to access data in Amazon S3 within an Amazon Virtual Private Cloud environment (Amazon VPC).
--
-- * 'stExclusions' - A list of glob patterns used to exclude from the crawl. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/add-crawler.html Catalog Tables with a Crawler> .
s3Target ::
  S3Target
s3Target =
  S3Target'
    { _stPath = Nothing,
      _stConnectionName = Nothing,
      _stExclusions = Nothing
    }

-- | The path to the Amazon S3 target.
stPath :: Lens' S3Target (Maybe Text)
stPath = lens _stPath (\s a -> s {_stPath = a})

-- | The name of a connection which allows a job or crawler to access data in Amazon S3 within an Amazon Virtual Private Cloud environment (Amazon VPC).
stConnectionName :: Lens' S3Target (Maybe Text)
stConnectionName = lens _stConnectionName (\s a -> s {_stConnectionName = a})

-- | A list of glob patterns used to exclude from the crawl. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/add-crawler.html Catalog Tables with a Crawler> .
stExclusions :: Lens' S3Target [Text]
stExclusions = lens _stExclusions (\s a -> s {_stExclusions = a}) . _Default . _Coerce

instance FromJSON S3Target where
  parseJSON =
    withObject
      "S3Target"
      ( \x ->
          S3Target'
            <$> (x .:? "Path")
            <*> (x .:? "ConnectionName")
            <*> (x .:? "Exclusions" .!= mempty)
      )

instance Hashable S3Target

instance NFData S3Target

instance ToJSON S3Target where
  toJSON S3Target' {..} =
    object
      ( catMaybes
          [ ("Path" .=) <$> _stPath,
            ("ConnectionName" .=) <$> _stConnectionName,
            ("Exclusions" .=) <$> _stExclusions
          ]
      )
