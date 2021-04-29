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
-- Module      : Network.AWS.WAF.Types.IPSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.IPSet where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.WAF.Types.IPSetDescriptor

-- | This is __AWS WAF Classic__ documentation. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic>
-- in the developer guide.
--
-- __For the latest version of AWS WAF__, use the AWS WAFV2 API and see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide>.
-- With the latest version, AWS WAF has a single set of endpoints for
-- regional and global use.
--
-- Contains one or more IP addresses or blocks of IP addresses specified in
-- Classless Inter-Domain Routing (CIDR) notation. AWS WAF supports IPv4
-- address ranges: \/8 and any range between \/16 through \/32. AWS WAF
-- supports IPv6 address ranges: \/24, \/32, \/48, \/56, \/64, and \/128.
--
-- To specify an individual IP address, you specify the four-part IP
-- address followed by a @\/32@, for example, 192.0.2.0\/32. To block a
-- range of IP addresses, you can specify \/8 or any range between \/16
-- through \/32 (for IPv4) or \/24, \/32, \/48, \/56, \/64, or \/128 (for
-- IPv6). For more information about CIDR notation, see the Wikipedia entry
-- <https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing Classless Inter-Domain Routing>.
--
-- /See:/ 'newIPSet' smart constructor.
data IPSet = IPSet'
  { -- | A friendly name or description of the IPSet. You can\'t change the name
    -- of an @IPSet@ after you create it.
    name :: Prelude.Maybe Prelude.Text,
    -- | The @IPSetId@ for an @IPSet@. You use @IPSetId@ to get information about
    -- an @IPSet@ (see GetIPSet), update an @IPSet@ (see UpdateIPSet), insert
    -- an @IPSet@ into a @Rule@ or delete one from a @Rule@ (see UpdateRule),
    -- and delete an @IPSet@ from AWS WAF (see DeleteIPSet).
    --
    -- @IPSetId@ is returned by CreateIPSet and by ListIPSets.
    iPSetId :: Prelude.Text,
    -- | The IP address type (@IPV4@ or @IPV6@) and the IP address range (in CIDR
    -- notation) that web requests originate from. If the @WebACL@ is
    -- associated with a CloudFront distribution and the viewer did not use an
    -- HTTP proxy or a load balancer to send the request, this is the value of
    -- the c-ip field in the CloudFront access logs.
    iPSetDescriptors :: [IPSetDescriptor]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'IPSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'iPSet_name' - A friendly name or description of the IPSet. You can\'t change the name
-- of an @IPSet@ after you create it.
--
-- 'iPSetId', 'iPSet_iPSetId' - The @IPSetId@ for an @IPSet@. You use @IPSetId@ to get information about
-- an @IPSet@ (see GetIPSet), update an @IPSet@ (see UpdateIPSet), insert
-- an @IPSet@ into a @Rule@ or delete one from a @Rule@ (see UpdateRule),
-- and delete an @IPSet@ from AWS WAF (see DeleteIPSet).
--
-- @IPSetId@ is returned by CreateIPSet and by ListIPSets.
--
-- 'iPSetDescriptors', 'iPSet_iPSetDescriptors' - The IP address type (@IPV4@ or @IPV6@) and the IP address range (in CIDR
-- notation) that web requests originate from. If the @WebACL@ is
-- associated with a CloudFront distribution and the viewer did not use an
-- HTTP proxy or a load balancer to send the request, this is the value of
-- the c-ip field in the CloudFront access logs.
newIPSet ::
  -- | 'iPSetId'
  Prelude.Text ->
  IPSet
newIPSet pIPSetId_ =
  IPSet'
    { name = Prelude.Nothing,
      iPSetId = pIPSetId_,
      iPSetDescriptors = Prelude.mempty
    }

-- | A friendly name or description of the IPSet. You can\'t change the name
-- of an @IPSet@ after you create it.
iPSet_name :: Lens.Lens' IPSet (Prelude.Maybe Prelude.Text)
iPSet_name = Lens.lens (\IPSet' {name} -> name) (\s@IPSet' {} a -> s {name = a} :: IPSet)

-- | The @IPSetId@ for an @IPSet@. You use @IPSetId@ to get information about
-- an @IPSet@ (see GetIPSet), update an @IPSet@ (see UpdateIPSet), insert
-- an @IPSet@ into a @Rule@ or delete one from a @Rule@ (see UpdateRule),
-- and delete an @IPSet@ from AWS WAF (see DeleteIPSet).
--
-- @IPSetId@ is returned by CreateIPSet and by ListIPSets.
iPSet_iPSetId :: Lens.Lens' IPSet Prelude.Text
iPSet_iPSetId = Lens.lens (\IPSet' {iPSetId} -> iPSetId) (\s@IPSet' {} a -> s {iPSetId = a} :: IPSet)

-- | The IP address type (@IPV4@ or @IPV6@) and the IP address range (in CIDR
-- notation) that web requests originate from. If the @WebACL@ is
-- associated with a CloudFront distribution and the viewer did not use an
-- HTTP proxy or a load balancer to send the request, this is the value of
-- the c-ip field in the CloudFront access logs.
iPSet_iPSetDescriptors :: Lens.Lens' IPSet [IPSetDescriptor]
iPSet_iPSetDescriptors = Lens.lens (\IPSet' {iPSetDescriptors} -> iPSetDescriptors) (\s@IPSet' {} a -> s {iPSetDescriptors = a} :: IPSet) Prelude.. Prelude._Coerce

instance Prelude.FromJSON IPSet where
  parseJSON =
    Prelude.withObject
      "IPSet"
      ( \x ->
          IPSet'
            Prelude.<$> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..: "IPSetId")
            Prelude.<*> ( x Prelude..:? "IPSetDescriptors"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable IPSet

instance Prelude.NFData IPSet
