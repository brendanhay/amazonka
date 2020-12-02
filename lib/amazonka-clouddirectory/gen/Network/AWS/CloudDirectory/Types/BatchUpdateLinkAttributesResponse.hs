{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchUpdateLinkAttributesResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchUpdateLinkAttributesResponse where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the output of a 'UpdateLinkAttributes' response operation.
--
--
--
-- /See:/ 'batchUpdateLinkAttributesResponse' smart constructor.
data BatchUpdateLinkAttributesResponse = BatchUpdateLinkAttributesResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchUpdateLinkAttributesResponse' with the minimum fields required to make a request.
batchUpdateLinkAttributesResponse ::
  BatchUpdateLinkAttributesResponse
batchUpdateLinkAttributesResponse =
  BatchUpdateLinkAttributesResponse'

instance FromJSON BatchUpdateLinkAttributesResponse where
  parseJSON =
    withObject
      "BatchUpdateLinkAttributesResponse"
      (\x -> pure BatchUpdateLinkAttributesResponse')

instance Hashable BatchUpdateLinkAttributesResponse

instance NFData BatchUpdateLinkAttributesResponse
