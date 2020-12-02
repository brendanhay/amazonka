{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.ArrayPropertiesDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.ArrayPropertiesDetail where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object representing the array properties of a job.
--
--
--
-- /See:/ 'arrayPropertiesDetail' smart constructor.
data ArrayPropertiesDetail = ArrayPropertiesDetail'
  { _apdSize ::
      !(Maybe Int),
    _apdStatusSummary :: !(Maybe (Map Text (Int))),
    _apdIndex :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ArrayPropertiesDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'apdSize' - The size of the array job. This parameter is returned for parent array jobs.
--
-- * 'apdStatusSummary' - A summary of the number of array job children in each available job status. This parameter is returned for parent array jobs.
--
-- * 'apdIndex' - The job index within the array that is associated with this job. This parameter is returned for array job children.
arrayPropertiesDetail ::
  ArrayPropertiesDetail
arrayPropertiesDetail =
  ArrayPropertiesDetail'
    { _apdSize = Nothing,
      _apdStatusSummary = Nothing,
      _apdIndex = Nothing
    }

-- | The size of the array job. This parameter is returned for parent array jobs.
apdSize :: Lens' ArrayPropertiesDetail (Maybe Int)
apdSize = lens _apdSize (\s a -> s {_apdSize = a})

-- | A summary of the number of array job children in each available job status. This parameter is returned for parent array jobs.
apdStatusSummary :: Lens' ArrayPropertiesDetail (HashMap Text (Int))
apdStatusSummary = lens _apdStatusSummary (\s a -> s {_apdStatusSummary = a}) . _Default . _Map

-- | The job index within the array that is associated with this job. This parameter is returned for array job children.
apdIndex :: Lens' ArrayPropertiesDetail (Maybe Int)
apdIndex = lens _apdIndex (\s a -> s {_apdIndex = a})

instance FromJSON ArrayPropertiesDetail where
  parseJSON =
    withObject
      "ArrayPropertiesDetail"
      ( \x ->
          ArrayPropertiesDetail'
            <$> (x .:? "size")
            <*> (x .:? "statusSummary" .!= mempty)
            <*> (x .:? "index")
      )

instance Hashable ArrayPropertiesDetail

instance NFData ArrayPropertiesDetail
