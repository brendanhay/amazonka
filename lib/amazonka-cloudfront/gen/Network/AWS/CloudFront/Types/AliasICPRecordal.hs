-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.AliasICPRecordal
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.AliasICPRecordal
  ( AliasICPRecordal (..),

    -- * Smart constructor
    mkAliasICPRecordal,

    -- * Lenses
    aicprCNAME,
    aicprICPRecordalStatus,
  )
where

import Network.AWS.CloudFront.Types.ICPRecordalStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | AWS services in China customers must file for an Internet Content Provider (ICP) recordal if they want to serve content publicly on an alternate domain name, also known as a CNAME, that they've added to CloudFront. AliasICPRecordal provides the ICP recordal status for CNAMEs associated with distributions. The status is returned in the CloudFront response; you can't configure it yourself.
--
-- For more information about ICP recordals, see <https://docs.amazonaws.cn/en_us/aws/latest/userguide/accounts-and-credentials.html Signup, Accounts, and Credentials> in /Getting Started with AWS services in China/ .
--
-- /See:/ 'mkAliasICPRecordal' smart constructor.
data AliasICPRecordal = AliasICPRecordal'
  { cNAME ::
      Lude.Maybe Lude.Text,
    iCPRecordalStatus :: Lude.Maybe ICPRecordalStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AliasICPRecordal' with the minimum fields required to make a request.
--
-- * 'cNAME' - A domain name associated with a distribution.
-- * 'iCPRecordalStatus' - The Internet Content Provider (ICP) recordal status for a CNAME. The ICPRecordalStatus is set to APPROVED for all CNAMEs (aliases) in regions outside of China.
--
-- The status values returned are the following:
--
--     * __APPROVED__ indicates that the associated CNAME has a valid ICP recordal number. Multiple CNAMEs can be associated with a distribution, and CNAMEs can correspond to different ICP recordals. To be marked as APPROVED, that is, valid to use with China region, a CNAME must have one ICP recordal number associated with it.
--
--
--     * __SUSPENDED__ indicates that the associated CNAME does not have a valid ICP recordal number.
--
--
--     * __PENDING__ indicates that CloudFront can't determine the ICP recordal status of the CNAME associated with the distribution because there was an error in trying to determine the status. You can try again to see if the error is resolved in which case CloudFront returns an APPROVED or SUSPENDED status.
mkAliasICPRecordal ::
  AliasICPRecordal
mkAliasICPRecordal =
  AliasICPRecordal'
    { cNAME = Lude.Nothing,
      iCPRecordalStatus = Lude.Nothing
    }

-- | A domain name associated with a distribution.
--
-- /Note:/ Consider using 'cNAME' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aicprCNAME :: Lens.Lens' AliasICPRecordal (Lude.Maybe Lude.Text)
aicprCNAME = Lens.lens (cNAME :: AliasICPRecordal -> Lude.Maybe Lude.Text) (\s a -> s {cNAME = a} :: AliasICPRecordal)
{-# DEPRECATED aicprCNAME "Use generic-lens or generic-optics with 'cNAME' instead." #-}

-- | The Internet Content Provider (ICP) recordal status for a CNAME. The ICPRecordalStatus is set to APPROVED for all CNAMEs (aliases) in regions outside of China.
--
-- The status values returned are the following:
--
--     * __APPROVED__ indicates that the associated CNAME has a valid ICP recordal number. Multiple CNAMEs can be associated with a distribution, and CNAMEs can correspond to different ICP recordals. To be marked as APPROVED, that is, valid to use with China region, a CNAME must have one ICP recordal number associated with it.
--
--
--     * __SUSPENDED__ indicates that the associated CNAME does not have a valid ICP recordal number.
--
--
--     * __PENDING__ indicates that CloudFront can't determine the ICP recordal status of the CNAME associated with the distribution because there was an error in trying to determine the status. You can try again to see if the error is resolved in which case CloudFront returns an APPROVED or SUSPENDED status.
--
--
--
-- /Note:/ Consider using 'iCPRecordalStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aicprICPRecordalStatus :: Lens.Lens' AliasICPRecordal (Lude.Maybe ICPRecordalStatus)
aicprICPRecordalStatus = Lens.lens (iCPRecordalStatus :: AliasICPRecordal -> Lude.Maybe ICPRecordalStatus) (\s a -> s {iCPRecordalStatus = a} :: AliasICPRecordal)
{-# DEPRECATED aicprICPRecordalStatus "Use generic-lens or generic-optics with 'iCPRecordalStatus' instead." #-}

instance Lude.FromXML AliasICPRecordal where
  parseXML x =
    AliasICPRecordal'
      Lude.<$> (x Lude..@? "CNAME") Lude.<*> (x Lude..@? "ICPRecordalStatus")
