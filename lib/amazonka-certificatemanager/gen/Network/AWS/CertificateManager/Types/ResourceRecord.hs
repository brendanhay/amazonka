{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types.ResourceRecord
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManager.Types.ResourceRecord
  ( ResourceRecord (..),

    -- * Smart constructor
    mkResourceRecord,

    -- * Lenses
    rrName,
    rrType,
    rrValue,
  )
where

import qualified Network.AWS.CertificateManager.Types.Name as Types
import qualified Network.AWS.CertificateManager.Types.RecordType as Types
import qualified Network.AWS.CertificateManager.Types.Value as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains a DNS record value that you can use to can use to validate ownership or control of a domain. This is used by the 'DescribeCertificate' action.
--
-- /See:/ 'mkResourceRecord' smart constructor.
data ResourceRecord = ResourceRecord'
  { -- | The name of the DNS record to create in your domain. This is supplied by ACM.
    name :: Types.Name,
    -- | The type of DNS record. Currently this can be @CNAME@ .
    type' :: Types.RecordType,
    -- | The value of the CNAME record to add to your DNS database. This is supplied by ACM.
    value :: Types.Value
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResourceRecord' value with any optional fields omitted.
mkResourceRecord ::
  -- | 'name'
  Types.Name ->
  -- | 'type\''
  Types.RecordType ->
  -- | 'value'
  Types.Value ->
  ResourceRecord
mkResourceRecord name type' value =
  ResourceRecord' {name, type', value}

-- | The name of the DNS record to create in your domain. This is supplied by ACM.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrName :: Lens.Lens' ResourceRecord Types.Name
rrName = Lens.field @"name"
{-# DEPRECATED rrName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The type of DNS record. Currently this can be @CNAME@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrType :: Lens.Lens' ResourceRecord Types.RecordType
rrType = Lens.field @"type'"
{-# DEPRECATED rrType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The value of the CNAME record to add to your DNS database. This is supplied by ACM.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrValue :: Lens.Lens' ResourceRecord Types.Value
rrValue = Lens.field @"value"
{-# DEPRECATED rrValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromJSON ResourceRecord where
  parseJSON =
    Core.withObject "ResourceRecord" Core.$
      \x ->
        ResourceRecord'
          Core.<$> (x Core..: "Name")
          Core.<*> (x Core..: "Type")
          Core.<*> (x Core..: "Value")
