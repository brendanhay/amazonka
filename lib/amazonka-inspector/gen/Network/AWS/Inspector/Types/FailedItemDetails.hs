{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.FailedItemDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.FailedItemDetails where

import Network.AWS.Inspector.Types.FailedItemErrorCode
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Includes details about the failed items.
--
--
--
-- /See:/ 'failedItemDetails' smart constructor.
data FailedItemDetails = FailedItemDetails'
  { _fidFailureCode ::
      !FailedItemErrorCode,
    _fidRetryable :: !Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FailedItemDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fidFailureCode' - The status code of a failed item.
--
-- * 'fidRetryable' - Indicates whether you can immediately retry a request for this item for a specified resource.
failedItemDetails ::
  -- | 'fidFailureCode'
  FailedItemErrorCode ->
  -- | 'fidRetryable'
  Bool ->
  FailedItemDetails
failedItemDetails pFailureCode_ pRetryable_ =
  FailedItemDetails'
    { _fidFailureCode = pFailureCode_,
      _fidRetryable = pRetryable_
    }

-- | The status code of a failed item.
fidFailureCode :: Lens' FailedItemDetails FailedItemErrorCode
fidFailureCode = lens _fidFailureCode (\s a -> s {_fidFailureCode = a})

-- | Indicates whether you can immediately retry a request for this item for a specified resource.
fidRetryable :: Lens' FailedItemDetails Bool
fidRetryable = lens _fidRetryable (\s a -> s {_fidRetryable = a})

instance FromJSON FailedItemDetails where
  parseJSON =
    withObject
      "FailedItemDetails"
      ( \x ->
          FailedItemDetails' <$> (x .: "failureCode") <*> (x .: "retryable")
      )

instance Hashable FailedItemDetails

instance NFData FailedItemDetails
