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
-- Module      : Amazonka.ConnectCases.Types.EventIncludedData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectCases.Types.EventIncludedData where

import Amazonka.ConnectCases.Types.CaseEventIncludedData
import Amazonka.ConnectCases.Types.RelatedItemEventIncludedData
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details of what case and related item data is published through the case
-- event stream.
--
-- /See:/ 'newEventIncludedData' smart constructor.
data EventIncludedData = EventIncludedData'
  { -- | Details of what case data is published through the case event stream.
    caseData :: Prelude.Maybe CaseEventIncludedData,
    -- | Details of what related item data is published through the case event
    -- stream.
    relatedItemData :: Prelude.Maybe RelatedItemEventIncludedData
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EventIncludedData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'caseData', 'eventIncludedData_caseData' - Details of what case data is published through the case event stream.
--
-- 'relatedItemData', 'eventIncludedData_relatedItemData' - Details of what related item data is published through the case event
-- stream.
newEventIncludedData ::
  EventIncludedData
newEventIncludedData =
  EventIncludedData'
    { caseData = Prelude.Nothing,
      relatedItemData = Prelude.Nothing
    }

-- | Details of what case data is published through the case event stream.
eventIncludedData_caseData :: Lens.Lens' EventIncludedData (Prelude.Maybe CaseEventIncludedData)
eventIncludedData_caseData = Lens.lens (\EventIncludedData' {caseData} -> caseData) (\s@EventIncludedData' {} a -> s {caseData = a} :: EventIncludedData)

-- | Details of what related item data is published through the case event
-- stream.
eventIncludedData_relatedItemData :: Lens.Lens' EventIncludedData (Prelude.Maybe RelatedItemEventIncludedData)
eventIncludedData_relatedItemData = Lens.lens (\EventIncludedData' {relatedItemData} -> relatedItemData) (\s@EventIncludedData' {} a -> s {relatedItemData = a} :: EventIncludedData)

instance Data.FromJSON EventIncludedData where
  parseJSON =
    Data.withObject
      "EventIncludedData"
      ( \x ->
          EventIncludedData'
            Prelude.<$> (x Data..:? "caseData")
            Prelude.<*> (x Data..:? "relatedItemData")
      )

instance Prelude.Hashable EventIncludedData where
  hashWithSalt _salt EventIncludedData' {..} =
    _salt
      `Prelude.hashWithSalt` caseData
      `Prelude.hashWithSalt` relatedItemData

instance Prelude.NFData EventIncludedData where
  rnf EventIncludedData' {..} =
    Prelude.rnf caseData `Prelude.seq`
      Prelude.rnf relatedItemData

instance Data.ToJSON EventIncludedData where
  toJSON EventIncludedData' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("caseData" Data..=) Prelude.<$> caseData,
            ("relatedItemData" Data..=)
              Prelude.<$> relatedItemData
          ]
      )
