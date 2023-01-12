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
-- Module      : Amazonka.XRay.Types.ValueWithServiceIds
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.XRay.Types.ValueWithServiceIds where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.XRay.Types.AnnotationValue
import Amazonka.XRay.Types.ServiceId

-- | Information about a segment annotation.
--
-- /See:/ 'newValueWithServiceIds' smart constructor.
data ValueWithServiceIds = ValueWithServiceIds'
  { -- | Values of the annotation.
    annotationValue :: Prelude.Maybe AnnotationValue,
    -- | Services to which the annotation applies.
    serviceIds :: Prelude.Maybe [ServiceId]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ValueWithServiceIds' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'annotationValue', 'valueWithServiceIds_annotationValue' - Values of the annotation.
--
-- 'serviceIds', 'valueWithServiceIds_serviceIds' - Services to which the annotation applies.
newValueWithServiceIds ::
  ValueWithServiceIds
newValueWithServiceIds =
  ValueWithServiceIds'
    { annotationValue =
        Prelude.Nothing,
      serviceIds = Prelude.Nothing
    }

-- | Values of the annotation.
valueWithServiceIds_annotationValue :: Lens.Lens' ValueWithServiceIds (Prelude.Maybe AnnotationValue)
valueWithServiceIds_annotationValue = Lens.lens (\ValueWithServiceIds' {annotationValue} -> annotationValue) (\s@ValueWithServiceIds' {} a -> s {annotationValue = a} :: ValueWithServiceIds)

-- | Services to which the annotation applies.
valueWithServiceIds_serviceIds :: Lens.Lens' ValueWithServiceIds (Prelude.Maybe [ServiceId])
valueWithServiceIds_serviceIds = Lens.lens (\ValueWithServiceIds' {serviceIds} -> serviceIds) (\s@ValueWithServiceIds' {} a -> s {serviceIds = a} :: ValueWithServiceIds) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ValueWithServiceIds where
  parseJSON =
    Data.withObject
      "ValueWithServiceIds"
      ( \x ->
          ValueWithServiceIds'
            Prelude.<$> (x Data..:? "AnnotationValue")
            Prelude.<*> (x Data..:? "ServiceIds" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ValueWithServiceIds where
  hashWithSalt _salt ValueWithServiceIds' {..} =
    _salt `Prelude.hashWithSalt` annotationValue
      `Prelude.hashWithSalt` serviceIds

instance Prelude.NFData ValueWithServiceIds where
  rnf ValueWithServiceIds' {..} =
    Prelude.rnf annotationValue
      `Prelude.seq` Prelude.rnf serviceIds
