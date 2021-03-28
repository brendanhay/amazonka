{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types.ResourceRecord
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CertificateManager.Types.ResourceRecord
  ( ResourceRecord (..)
  -- * Smart constructor
  , mkResourceRecord
  -- * Lenses
  , rrName
  , rrType
  , rrValue
  ) where

import qualified Network.AWS.CertificateManager.Types.RecordType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains a DNS record value that you can use to can use to validate ownership or control of a domain. This is used by the 'DescribeCertificate' action. 
--
-- /See:/ 'mkResourceRecord' smart constructor.
data ResourceRecord = ResourceRecord'
  { name :: Core.Text
    -- ^ The name of the DNS record to create in your domain. This is supplied by ACM.
  , type' :: Types.RecordType
    -- ^ The type of DNS record. Currently this can be @CNAME@ .
  , value :: Core.Text
    -- ^ The value of the CNAME record to add to your DNS database. This is supplied by ACM.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResourceRecord' value with any optional fields omitted.
mkResourceRecord
    :: Core.Text -- ^ 'name'
    -> Types.RecordType -- ^ 'type\''
    -> Core.Text -- ^ 'value'
    -> ResourceRecord
mkResourceRecord name type' value
  = ResourceRecord'{name, type', value}

-- | The name of the DNS record to create in your domain. This is supplied by ACM.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrName :: Lens.Lens' ResourceRecord Core.Text
rrName = Lens.field @"name"
{-# INLINEABLE rrName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The type of DNS record. Currently this can be @CNAME@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrType :: Lens.Lens' ResourceRecord Types.RecordType
rrType = Lens.field @"type'"
{-# INLINEABLE rrType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The value of the CNAME record to add to your DNS database. This is supplied by ACM.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrValue :: Lens.Lens' ResourceRecord Core.Text
rrValue = Lens.field @"value"
{-# INLINEABLE rrValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromJSON ResourceRecord where
        parseJSON
          = Core.withObject "ResourceRecord" Core.$
              \ x ->
                ResourceRecord' Core.<$>
                  (x Core..: "Name") Core.<*> x Core..: "Type" Core.<*>
                    x Core..: "Value"
