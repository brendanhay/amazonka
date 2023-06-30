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
-- Module      : Amazonka.CloudFront.Types.FunctionAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.FunctionAssociation where

import Amazonka.CloudFront.Types.EventType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A CloudFront function that is associated with a cache behavior in a
-- CloudFront distribution.
--
-- /See:/ 'newFunctionAssociation' smart constructor.
data FunctionAssociation = FunctionAssociation'
  { -- | The Amazon Resource Name (ARN) of the function.
    functionARN :: Prelude.Text,
    -- | The event type of the function, either @viewer-request@ or
    -- @viewer-response@. You cannot use origin-facing event types
    -- (@origin-request@ and @origin-response@) with a CloudFront function.
    eventType :: EventType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FunctionAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'functionARN', 'functionAssociation_functionARN' - The Amazon Resource Name (ARN) of the function.
--
-- 'eventType', 'functionAssociation_eventType' - The event type of the function, either @viewer-request@ or
-- @viewer-response@. You cannot use origin-facing event types
-- (@origin-request@ and @origin-response@) with a CloudFront function.
newFunctionAssociation ::
  -- | 'functionARN'
  Prelude.Text ->
  -- | 'eventType'
  EventType ->
  FunctionAssociation
newFunctionAssociation pFunctionARN_ pEventType_ =
  FunctionAssociation'
    { functionARN = pFunctionARN_,
      eventType = pEventType_
    }

-- | The Amazon Resource Name (ARN) of the function.
functionAssociation_functionARN :: Lens.Lens' FunctionAssociation Prelude.Text
functionAssociation_functionARN = Lens.lens (\FunctionAssociation' {functionARN} -> functionARN) (\s@FunctionAssociation' {} a -> s {functionARN = a} :: FunctionAssociation)

-- | The event type of the function, either @viewer-request@ or
-- @viewer-response@. You cannot use origin-facing event types
-- (@origin-request@ and @origin-response@) with a CloudFront function.
functionAssociation_eventType :: Lens.Lens' FunctionAssociation EventType
functionAssociation_eventType = Lens.lens (\FunctionAssociation' {eventType} -> eventType) (\s@FunctionAssociation' {} a -> s {eventType = a} :: FunctionAssociation)

instance Data.FromXML FunctionAssociation where
  parseXML x =
    FunctionAssociation'
      Prelude.<$> (x Data..@ "FunctionARN")
      Prelude.<*> (x Data..@ "EventType")

instance Prelude.Hashable FunctionAssociation where
  hashWithSalt _salt FunctionAssociation' {..} =
    _salt
      `Prelude.hashWithSalt` functionARN
      `Prelude.hashWithSalt` eventType

instance Prelude.NFData FunctionAssociation where
  rnf FunctionAssociation' {..} =
    Prelude.rnf functionARN
      `Prelude.seq` Prelude.rnf eventType

instance Data.ToXML FunctionAssociation where
  toXML FunctionAssociation' {..} =
    Prelude.mconcat
      [ "FunctionARN" Data.@= functionARN,
        "EventType" Data.@= eventType
      ]
