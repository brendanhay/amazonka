{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.OpsResultAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.OpsResultAttribute
  ( OpsResultAttribute (..)
  -- * Smart constructor
  , mkOpsResultAttribute
  -- * Lenses
  , oraTypeName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.OpsDataTypeName as Types

-- | The OpsItem data type to return.
--
-- /See:/ 'mkOpsResultAttribute' smart constructor.
newtype OpsResultAttribute = OpsResultAttribute'
  { typeName :: Types.OpsDataTypeName
    -- ^ Name of the data type. Valid value: AWS:OpsItem, AWS:EC2InstanceInformation, AWS:OpsItemTrendline, or AWS:ComplianceSummary.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'OpsResultAttribute' value with any optional fields omitted.
mkOpsResultAttribute
    :: Types.OpsDataTypeName -- ^ 'typeName'
    -> OpsResultAttribute
mkOpsResultAttribute typeName = OpsResultAttribute'{typeName}

-- | Name of the data type. Valid value: AWS:OpsItem, AWS:EC2InstanceInformation, AWS:OpsItemTrendline, or AWS:ComplianceSummary.
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oraTypeName :: Lens.Lens' OpsResultAttribute Types.OpsDataTypeName
oraTypeName = Lens.field @"typeName"
{-# INLINEABLE oraTypeName #-}
{-# DEPRECATED typeName "Use generic-lens or generic-optics with 'typeName' instead"  #-}

instance Core.FromJSON OpsResultAttribute where
        toJSON OpsResultAttribute{..}
          = Core.object
              (Core.catMaybes [Core.Just ("TypeName" Core..= typeName)])
