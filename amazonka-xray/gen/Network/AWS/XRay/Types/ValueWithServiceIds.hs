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
-- Module      : Network.AWS.XRay.Types.ValueWithServiceIds
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.ValueWithServiceIds where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.XRay.Types.AnnotationValue
import Network.AWS.XRay.Types.ServiceId

-- | Information about a segment annotation.
--
-- /See:/ 'newValueWithServiceIds' smart constructor.
data ValueWithServiceIds = ValueWithServiceIds'
  { -- | Values of the annotation.
    annotationValue :: Core.Maybe AnnotationValue,
    -- | Services to which the annotation applies.
    serviceIds :: Core.Maybe [ServiceId]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      serviceIds = Core.Nothing
    }

-- | Values of the annotation.
valueWithServiceIds_annotationValue :: Lens.Lens' ValueWithServiceIds (Core.Maybe AnnotationValue)
valueWithServiceIds_annotationValue = Lens.lens (\ValueWithServiceIds' {annotationValue} -> annotationValue) (\s@ValueWithServiceIds' {} a -> s {annotationValue = a} :: ValueWithServiceIds)

-- | Services to which the annotation applies.
valueWithServiceIds_serviceIds :: Lens.Lens' ValueWithServiceIds (Core.Maybe [ServiceId])
valueWithServiceIds_serviceIds = Lens.lens (\ValueWithServiceIds' {serviceIds} -> serviceIds) (\s@ValueWithServiceIds' {} a -> s {serviceIds = a} :: ValueWithServiceIds) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON ValueWithServiceIds where
  parseJSON =
    Core.withObject
      "ValueWithServiceIds"
      ( \x ->
          ValueWithServiceIds'
            Core.<$> (x Core..:? "AnnotationValue")
            Core.<*> (x Core..:? "ServiceIds" Core..!= Core.mempty)
      )

instance Core.Hashable ValueWithServiceIds

instance Core.NFData ValueWithServiceIds
