{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchDetachTypedLinkResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchDetachTypedLinkResponse where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the output of a DetachTypedLink response operation.
--
-- /See:/ 'newBatchDetachTypedLinkResponse' smart constructor.
data BatchDetachTypedLinkResponse = BatchDetachTypedLinkResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BatchDetachTypedLinkResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newBatchDetachTypedLinkResponse ::
  BatchDetachTypedLinkResponse
newBatchDetachTypedLinkResponse =
  BatchDetachTypedLinkResponse'

instance
  Prelude.FromJSON
    BatchDetachTypedLinkResponse
  where
  parseJSON =
    Prelude.withObject
      "BatchDetachTypedLinkResponse"
      (\x -> Prelude.pure BatchDetachTypedLinkResponse')

instance
  Prelude.Hashable
    BatchDetachTypedLinkResponse

instance Prelude.NFData BatchDetachTypedLinkResponse
