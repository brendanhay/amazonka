{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.ExportFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.ExportFilter where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Used to select which agent's data is to be exported. A single agent ID may be selected for export using the <http://docs.aws.amazon.com/application-discovery/latest/APIReference/API_StartExportTask.html StartExportTask> action.
--
--
--
-- /See:/ 'exportFilter' smart constructor.
data ExportFilter = ExportFilter'
  { _efName :: !Text,
    _efValues :: ![Text],
    _efCondition :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExportFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'efName' - A single @ExportFilter@ name. Supported filters: @agentId@ .
--
-- * 'efValues' - A single @agentId@ for a Discovery Agent. An @agentId@ can be found using the <http://docs.aws.amazon.com/application-discovery/latest/APIReference/API_DescribeExportTasks.html DescribeAgents> action. Typically an ADS @agentId@ is in the form @o-0123456789abcdef0@ .
--
-- * 'efCondition' - Supported condition: @EQUALS@
exportFilter ::
  -- | 'efName'
  Text ->
  -- | 'efCondition'
  Text ->
  ExportFilter
exportFilter pName_ pCondition_ =
  ExportFilter'
    { _efName = pName_,
      _efValues = mempty,
      _efCondition = pCondition_
    }

-- | A single @ExportFilter@ name. Supported filters: @agentId@ .
efName :: Lens' ExportFilter Text
efName = lens _efName (\s a -> s {_efName = a})

-- | A single @agentId@ for a Discovery Agent. An @agentId@ can be found using the <http://docs.aws.amazon.com/application-discovery/latest/APIReference/API_DescribeExportTasks.html DescribeAgents> action. Typically an ADS @agentId@ is in the form @o-0123456789abcdef0@ .
efValues :: Lens' ExportFilter [Text]
efValues = lens _efValues (\s a -> s {_efValues = a}) . _Coerce

-- | Supported condition: @EQUALS@
efCondition :: Lens' ExportFilter Text
efCondition = lens _efCondition (\s a -> s {_efCondition = a})

instance Hashable ExportFilter

instance NFData ExportFilter

instance ToJSON ExportFilter where
  toJSON ExportFilter' {..} =
    object
      ( catMaybes
          [ Just ("name" .= _efName),
            Just ("values" .= _efValues),
            Just ("condition" .= _efCondition)
          ]
      )
