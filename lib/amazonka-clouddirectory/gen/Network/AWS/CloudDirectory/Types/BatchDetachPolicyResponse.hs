{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchDetachPolicyResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchDetachPolicyResponse where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the output of a 'DetachPolicy' response operation.
--
--
--
-- /See:/ 'batchDetachPolicyResponse' smart constructor.
data BatchDetachPolicyResponse = BatchDetachPolicyResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchDetachPolicyResponse' with the minimum fields required to make a request.
batchDetachPolicyResponse ::
  BatchDetachPolicyResponse
batchDetachPolicyResponse = BatchDetachPolicyResponse'

instance FromJSON BatchDetachPolicyResponse where
  parseJSON =
    withObject
      "BatchDetachPolicyResponse"
      (\x -> pure BatchDetachPolicyResponse')

instance Hashable BatchDetachPolicyResponse

instance NFData BatchDetachPolicyResponse
