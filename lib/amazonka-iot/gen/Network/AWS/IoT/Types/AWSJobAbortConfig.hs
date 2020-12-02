{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AWSJobAbortConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AWSJobAbortConfig where

import Network.AWS.IoT.Types.AWSJobAbortCriteria
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The criteria that determine when and how a job abort takes place.
--
--
--
-- /See:/ 'awsJobAbortConfig' smart constructor.
newtype AWSJobAbortConfig = AWSJobAbortConfig'
  { _ajacAbortCriteriaList ::
      List1 AWSJobAbortCriteria
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AWSJobAbortConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ajacAbortCriteriaList' - The list of criteria that determine when and how to abort the job.
awsJobAbortConfig ::
  -- | 'ajacAbortCriteriaList'
  NonEmpty AWSJobAbortCriteria ->
  AWSJobAbortConfig
awsJobAbortConfig pAbortCriteriaList_ =
  AWSJobAbortConfig'
    { _ajacAbortCriteriaList =
        _List1 # pAbortCriteriaList_
    }

-- | The list of criteria that determine when and how to abort the job.
ajacAbortCriteriaList :: Lens' AWSJobAbortConfig (NonEmpty AWSJobAbortCriteria)
ajacAbortCriteriaList = lens _ajacAbortCriteriaList (\s a -> s {_ajacAbortCriteriaList = a}) . _List1

instance Hashable AWSJobAbortConfig

instance NFData AWSJobAbortConfig

instance ToJSON AWSJobAbortConfig where
  toJSON AWSJobAbortConfig' {..} =
    object
      (catMaybes [Just ("abortCriteriaList" .= _ajacAbortCriteriaList)])
