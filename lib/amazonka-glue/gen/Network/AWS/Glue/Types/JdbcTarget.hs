{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.JdbcTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.JdbcTarget where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies a JDBC data store to crawl.
--
--
--
-- /See:/ 'jdbcTarget' smart constructor.
data JdbcTarget = JdbcTarget'
  { _jtPath :: !(Maybe Text),
    _jtConnectionName :: !(Maybe Text),
    _jtExclusions :: !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'JdbcTarget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jtPath' - The path of the JDBC target.
--
-- * 'jtConnectionName' - The name of the connection to use to connect to the JDBC target.
--
-- * 'jtExclusions' - A list of glob patterns used to exclude from the crawl. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/add-crawler.html Catalog Tables with a Crawler> .
jdbcTarget ::
  JdbcTarget
jdbcTarget =
  JdbcTarget'
    { _jtPath = Nothing,
      _jtConnectionName = Nothing,
      _jtExclusions = Nothing
    }

-- | The path of the JDBC target.
jtPath :: Lens' JdbcTarget (Maybe Text)
jtPath = lens _jtPath (\s a -> s {_jtPath = a})

-- | The name of the connection to use to connect to the JDBC target.
jtConnectionName :: Lens' JdbcTarget (Maybe Text)
jtConnectionName = lens _jtConnectionName (\s a -> s {_jtConnectionName = a})

-- | A list of glob patterns used to exclude from the crawl. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/add-crawler.html Catalog Tables with a Crawler> .
jtExclusions :: Lens' JdbcTarget [Text]
jtExclusions = lens _jtExclusions (\s a -> s {_jtExclusions = a}) . _Default . _Coerce

instance FromJSON JdbcTarget where
  parseJSON =
    withObject
      "JdbcTarget"
      ( \x ->
          JdbcTarget'
            <$> (x .:? "Path")
            <*> (x .:? "ConnectionName")
            <*> (x .:? "Exclusions" .!= mempty)
      )

instance Hashable JdbcTarget

instance NFData JdbcTarget

instance ToJSON JdbcTarget where
  toJSON JdbcTarget' {..} =
    object
      ( catMaybes
          [ ("Path" .=) <$> _jtPath,
            ("ConnectionName" .=) <$> _jtConnectionName,
            ("Exclusions" .=) <$> _jtExclusions
          ]
      )
