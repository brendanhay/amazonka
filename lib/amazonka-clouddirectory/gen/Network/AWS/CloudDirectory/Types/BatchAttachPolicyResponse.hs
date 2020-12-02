{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchAttachPolicyResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchAttachPolicyResponse where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the output of an 'AttachPolicy' response operation.
--
--
--
-- /See:/ 'batchAttachPolicyResponse' smart constructor.
data BatchAttachPolicyResponse = BatchAttachPolicyResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchAttachPolicyResponse' with the minimum fields required to make a request.
batchAttachPolicyResponse ::
  BatchAttachPolicyResponse
batchAttachPolicyResponse = BatchAttachPolicyResponse'

instance FromJSON BatchAttachPolicyResponse where
  parseJSON =
    withObject
      "BatchAttachPolicyResponse"
      (\x -> pure BatchAttachPolicyResponse')

instance Hashable BatchAttachPolicyResponse

instance NFData BatchAttachPolicyResponse
