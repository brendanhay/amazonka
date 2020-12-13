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
    rrValue,
    rrName,
    rrType,
  )
where

import Network.AWS.CertificateManager.Types.RecordType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains a DNS record value that you can use to can use to validate ownership or control of a domain. This is used by the 'DescribeCertificate' action.
--
-- /See:/ 'mkResourceRecord' smart constructor.
data ResourceRecord = ResourceRecord'
  { -- | The value of the CNAME record to add to your DNS database. This is supplied by ACM.
    value :: Lude.Text,
    -- | The name of the DNS record to create in your domain. This is supplied by ACM.
    name :: Lude.Text,
    -- | The type of DNS record. Currently this can be @CNAME@ .
    type' :: RecordType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceRecord' with the minimum fields required to make a request.
--
-- * 'value' - The value of the CNAME record to add to your DNS database. This is supplied by ACM.
-- * 'name' - The name of the DNS record to create in your domain. This is supplied by ACM.
-- * 'type'' - The type of DNS record. Currently this can be @CNAME@ .
mkResourceRecord ::
  -- | 'value'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  -- | 'type''
  RecordType ->
  ResourceRecord
mkResourceRecord pValue_ pName_ pType_ =
  ResourceRecord' {value = pValue_, name = pName_, type' = pType_}

-- | The value of the CNAME record to add to your DNS database. This is supplied by ACM.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrValue :: Lens.Lens' ResourceRecord Lude.Text
rrValue = Lens.lens (value :: ResourceRecord -> Lude.Text) (\s a -> s {value = a} :: ResourceRecord)
{-# DEPRECATED rrValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The name of the DNS record to create in your domain. This is supplied by ACM.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrName :: Lens.Lens' ResourceRecord Lude.Text
rrName = Lens.lens (name :: ResourceRecord -> Lude.Text) (\s a -> s {name = a} :: ResourceRecord)
{-# DEPRECATED rrName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The type of DNS record. Currently this can be @CNAME@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrType :: Lens.Lens' ResourceRecord RecordType
rrType = Lens.lens (type' :: ResourceRecord -> RecordType) (\s a -> s {type' = a} :: ResourceRecord)
{-# DEPRECATED rrType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON ResourceRecord where
  parseJSON =
    Lude.withObject
      "ResourceRecord"
      ( \x ->
          ResourceRecord'
            Lude.<$> (x Lude..: "Value")
            Lude.<*> (x Lude..: "Name")
            Lude.<*> (x Lude..: "Type")
      )
