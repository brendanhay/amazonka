{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.SourceConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.SourceConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A specification for an environment configuration.
--
--
--
-- /See:/ 'sourceConfiguration' smart constructor.
data SourceConfiguration = SourceConfiguration'
  { _scTemplateName ::
      !(Maybe Text),
    _scApplicationName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SourceConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scTemplateName' - The name of the configuration template.
--
-- * 'scApplicationName' - The name of the application associated with the configuration.
sourceConfiguration ::
  SourceConfiguration
sourceConfiguration =
  SourceConfiguration'
    { _scTemplateName = Nothing,
      _scApplicationName = Nothing
    }

-- | The name of the configuration template.
scTemplateName :: Lens' SourceConfiguration (Maybe Text)
scTemplateName = lens _scTemplateName (\s a -> s {_scTemplateName = a})

-- | The name of the application associated with the configuration.
scApplicationName :: Lens' SourceConfiguration (Maybe Text)
scApplicationName = lens _scApplicationName (\s a -> s {_scApplicationName = a})

instance Hashable SourceConfiguration

instance NFData SourceConfiguration

instance ToQuery SourceConfiguration where
  toQuery SourceConfiguration' {..} =
    mconcat
      [ "TemplateName" =: _scTemplateName,
        "ApplicationName" =: _scApplicationName
      ]
