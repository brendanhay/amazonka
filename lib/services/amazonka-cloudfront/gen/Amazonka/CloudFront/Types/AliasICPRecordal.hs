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
-- Module      : Amazonka.CloudFront.Types.AliasICPRecordal
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.AliasICPRecordal where

import Amazonka.CloudFront.Types.ICPRecordalStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Amazon Web Services services in China customers must file for an
-- Internet Content Provider (ICP) recordal if they want to serve content
-- publicly on an alternate domain name, also known as a CNAME, that
-- they\'ve added to CloudFront. AliasICPRecordal provides the ICP recordal
-- status for CNAMEs associated with distributions. The status is returned
-- in the CloudFront response; you can\'t configure it yourself.
--
-- For more information about ICP recordals, see
-- <https://docs.amazonaws.cn/en_us/aws/latest/userguide/accounts-and-credentials.html Signup, Accounts, and Credentials>
-- in /Getting Started with Amazon Web Services services in China/.
--
-- /See:/ 'newAliasICPRecordal' smart constructor.
data AliasICPRecordal = AliasICPRecordal'
  { -- | A domain name associated with a distribution.
    cname :: Prelude.Maybe Prelude.Text,
    -- | The Internet Content Provider (ICP) recordal status for a CNAME. The
    -- ICPRecordalStatus is set to APPROVED for all CNAMEs (aliases) in regions
    -- outside of China.
    --
    -- The status values returned are the following:
    --
    -- -   __APPROVED__ indicates that the associated CNAME has a valid ICP
    --     recordal number. Multiple CNAMEs can be associated with a
    --     distribution, and CNAMEs can correspond to different ICP recordals.
    --     To be marked as APPROVED, that is, valid to use with China region, a
    --     CNAME must have one ICP recordal number associated with it.
    --
    -- -   __SUSPENDED__ indicates that the associated CNAME does not have a
    --     valid ICP recordal number.
    --
    -- -   __PENDING__ indicates that CloudFront can\'t determine the ICP
    --     recordal status of the CNAME associated with the distribution
    --     because there was an error in trying to determine the status. You
    --     can try again to see if the error is resolved in which case
    --     CloudFront returns an APPROVED or SUSPENDED status.
    iCPRecordalStatus :: Prelude.Maybe ICPRecordalStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AliasICPRecordal' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cname', 'aliasICPRecordal_cname' - A domain name associated with a distribution.
--
-- 'iCPRecordalStatus', 'aliasICPRecordal_iCPRecordalStatus' - The Internet Content Provider (ICP) recordal status for a CNAME. The
-- ICPRecordalStatus is set to APPROVED for all CNAMEs (aliases) in regions
-- outside of China.
--
-- The status values returned are the following:
--
-- -   __APPROVED__ indicates that the associated CNAME has a valid ICP
--     recordal number. Multiple CNAMEs can be associated with a
--     distribution, and CNAMEs can correspond to different ICP recordals.
--     To be marked as APPROVED, that is, valid to use with China region, a
--     CNAME must have one ICP recordal number associated with it.
--
-- -   __SUSPENDED__ indicates that the associated CNAME does not have a
--     valid ICP recordal number.
--
-- -   __PENDING__ indicates that CloudFront can\'t determine the ICP
--     recordal status of the CNAME associated with the distribution
--     because there was an error in trying to determine the status. You
--     can try again to see if the error is resolved in which case
--     CloudFront returns an APPROVED or SUSPENDED status.
newAliasICPRecordal ::
  AliasICPRecordal
newAliasICPRecordal =
  AliasICPRecordal'
    { cname = Prelude.Nothing,
      iCPRecordalStatus = Prelude.Nothing
    }

-- | A domain name associated with a distribution.
aliasICPRecordal_cname :: Lens.Lens' AliasICPRecordal (Prelude.Maybe Prelude.Text)
aliasICPRecordal_cname = Lens.lens (\AliasICPRecordal' {cname} -> cname) (\s@AliasICPRecordal' {} a -> s {cname = a} :: AliasICPRecordal)

-- | The Internet Content Provider (ICP) recordal status for a CNAME. The
-- ICPRecordalStatus is set to APPROVED for all CNAMEs (aliases) in regions
-- outside of China.
--
-- The status values returned are the following:
--
-- -   __APPROVED__ indicates that the associated CNAME has a valid ICP
--     recordal number. Multiple CNAMEs can be associated with a
--     distribution, and CNAMEs can correspond to different ICP recordals.
--     To be marked as APPROVED, that is, valid to use with China region, a
--     CNAME must have one ICP recordal number associated with it.
--
-- -   __SUSPENDED__ indicates that the associated CNAME does not have a
--     valid ICP recordal number.
--
-- -   __PENDING__ indicates that CloudFront can\'t determine the ICP
--     recordal status of the CNAME associated with the distribution
--     because there was an error in trying to determine the status. You
--     can try again to see if the error is resolved in which case
--     CloudFront returns an APPROVED or SUSPENDED status.
aliasICPRecordal_iCPRecordalStatus :: Lens.Lens' AliasICPRecordal (Prelude.Maybe ICPRecordalStatus)
aliasICPRecordal_iCPRecordalStatus = Lens.lens (\AliasICPRecordal' {iCPRecordalStatus} -> iCPRecordalStatus) (\s@AliasICPRecordal' {} a -> s {iCPRecordalStatus = a} :: AliasICPRecordal)

instance Data.FromXML AliasICPRecordal where
  parseXML x =
    AliasICPRecordal'
      Prelude.<$> (x Data..@? "CNAME")
      Prelude.<*> (x Data..@? "ICPRecordalStatus")

instance Prelude.Hashable AliasICPRecordal where
  hashWithSalt _salt AliasICPRecordal' {..} =
    _salt `Prelude.hashWithSalt` cname
      `Prelude.hashWithSalt` iCPRecordalStatus

instance Prelude.NFData AliasICPRecordal where
  rnf AliasICPRecordal' {..} =
    Prelude.rnf cname
      `Prelude.seq` Prelude.rnf iCPRecordalStatus
