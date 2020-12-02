{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.BatchSuccessfulResultModel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.BatchSuccessfulResultModel where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details from a successful operation
--
-- /See:/ 'batchSuccessfulResultModel' smart constructor.
data BatchSuccessfulResultModel = BatchSuccessfulResultModel'
  { _bsrmState ::
      !(Maybe Text),
    _bsrmARN :: !(Maybe Text),
    _bsrmId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchSuccessfulResultModel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bsrmState' - Current state of the resource
--
-- * 'bsrmARN' - ARN of the resource
--
-- * 'bsrmId' - ID of the resource
batchSuccessfulResultModel ::
  BatchSuccessfulResultModel
batchSuccessfulResultModel =
  BatchSuccessfulResultModel'
    { _bsrmState = Nothing,
      _bsrmARN = Nothing,
      _bsrmId = Nothing
    }

-- | Current state of the resource
bsrmState :: Lens' BatchSuccessfulResultModel (Maybe Text)
bsrmState = lens _bsrmState (\s a -> s {_bsrmState = a})

-- | ARN of the resource
bsrmARN :: Lens' BatchSuccessfulResultModel (Maybe Text)
bsrmARN = lens _bsrmARN (\s a -> s {_bsrmARN = a})

-- | ID of the resource
bsrmId :: Lens' BatchSuccessfulResultModel (Maybe Text)
bsrmId = lens _bsrmId (\s a -> s {_bsrmId = a})

instance FromJSON BatchSuccessfulResultModel where
  parseJSON =
    withObject
      "BatchSuccessfulResultModel"
      ( \x ->
          BatchSuccessfulResultModel'
            <$> (x .:? "state") <*> (x .:? "arn") <*> (x .:? "id")
      )

instance Hashable BatchSuccessfulResultModel

instance NFData BatchSuccessfulResultModel
