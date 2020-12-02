{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.BatchFailedResultModel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.BatchFailedResultModel where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details from a failed operation
--
-- /See:/ 'batchFailedResultModel' smart constructor.
data BatchFailedResultModel = BatchFailedResultModel'
  { _bfrmARN ::
      !(Maybe Text),
    _bfrmId :: !(Maybe Text),
    _bfrmCode :: !(Maybe Text),
    _bfrmMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchFailedResultModel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bfrmARN' - ARN of the resource
--
-- * 'bfrmId' - ID of the resource
--
-- * 'bfrmCode' - Error code for the failed operation
--
-- * 'bfrmMessage' - Error message for the failed operation
batchFailedResultModel ::
  BatchFailedResultModel
batchFailedResultModel =
  BatchFailedResultModel'
    { _bfrmARN = Nothing,
      _bfrmId = Nothing,
      _bfrmCode = Nothing,
      _bfrmMessage = Nothing
    }

-- | ARN of the resource
bfrmARN :: Lens' BatchFailedResultModel (Maybe Text)
bfrmARN = lens _bfrmARN (\s a -> s {_bfrmARN = a})

-- | ID of the resource
bfrmId :: Lens' BatchFailedResultModel (Maybe Text)
bfrmId = lens _bfrmId (\s a -> s {_bfrmId = a})

-- | Error code for the failed operation
bfrmCode :: Lens' BatchFailedResultModel (Maybe Text)
bfrmCode = lens _bfrmCode (\s a -> s {_bfrmCode = a})

-- | Error message for the failed operation
bfrmMessage :: Lens' BatchFailedResultModel (Maybe Text)
bfrmMessage = lens _bfrmMessage (\s a -> s {_bfrmMessage = a})

instance FromJSON BatchFailedResultModel where
  parseJSON =
    withObject
      "BatchFailedResultModel"
      ( \x ->
          BatchFailedResultModel'
            <$> (x .:? "arn")
            <*> (x .:? "id")
            <*> (x .:? "code")
            <*> (x .:? "message")
      )

instance Hashable BatchFailedResultModel

instance NFData BatchFailedResultModel
