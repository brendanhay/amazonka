{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.AliasICPRecordal
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.AliasICPRecordal where

import Network.AWS.CloudFront.Types.ICPRecordalStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | AWS services in China customers must file for an Internet Content Provider (ICP) recordal if they want to serve content publicly on an alternate domain name, also known as a CNAME, that they've added to CloudFront. AliasICPRecordal provides the ICP recordal status for CNAMEs associated with distributions. The status is returned in the CloudFront response; you can't configure it yourself.
--
--
-- For more information about ICP recordals, see <https://docs.amazonaws.cn/en_us/aws/latest/userguide/accounts-and-credentials.html Signup, Accounts, and Credentials> in /Getting Started with AWS services in China/ .
--
--
-- /See:/ 'aliasICPRecordal' smart constructor.
data AliasICPRecordal = AliasICPRecordal'
  { _aicprCNAME ::
      !(Maybe Text),
    _aicprICPRecordalStatus :: !(Maybe ICPRecordalStatus)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AliasICPRecordal' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aicprCNAME' - A domain name associated with a distribution.
--
-- * 'aicprICPRecordalStatus' - The Internet Content Provider (ICP) recordal status for a CNAME. The ICPRecordalStatus is set to APPROVED for all CNAMEs (aliases) in regions outside of China.  The status values returned are the following:     * __APPROVED__ indicates that the associated CNAME has a valid ICP recordal number. Multiple CNAMEs can be associated with a distribution, and CNAMEs can correspond to different ICP recordals. To be marked as APPROVED, that is, valid to use with China region, a CNAME must have one ICP recordal number associated with it.     * __SUSPENDED__ indicates that the associated CNAME does not have a valid ICP recordal number.     * __PENDING__ indicates that CloudFront can't determine the ICP recordal status of the CNAME associated with the distribution because there was an error in trying to determine the status. You can try again to see if the error is resolved in which case CloudFront returns an APPROVED or SUSPENDED status.
aliasICPRecordal ::
  AliasICPRecordal
aliasICPRecordal =
  AliasICPRecordal'
    { _aicprCNAME = Nothing,
      _aicprICPRecordalStatus = Nothing
    }

-- | A domain name associated with a distribution.
aicprCNAME :: Lens' AliasICPRecordal (Maybe Text)
aicprCNAME = lens _aicprCNAME (\s a -> s {_aicprCNAME = a})

-- | The Internet Content Provider (ICP) recordal status for a CNAME. The ICPRecordalStatus is set to APPROVED for all CNAMEs (aliases) in regions outside of China.  The status values returned are the following:     * __APPROVED__ indicates that the associated CNAME has a valid ICP recordal number. Multiple CNAMEs can be associated with a distribution, and CNAMEs can correspond to different ICP recordals. To be marked as APPROVED, that is, valid to use with China region, a CNAME must have one ICP recordal number associated with it.     * __SUSPENDED__ indicates that the associated CNAME does not have a valid ICP recordal number.     * __PENDING__ indicates that CloudFront can't determine the ICP recordal status of the CNAME associated with the distribution because there was an error in trying to determine the status. You can try again to see if the error is resolved in which case CloudFront returns an APPROVED or SUSPENDED status.
aicprICPRecordalStatus :: Lens' AliasICPRecordal (Maybe ICPRecordalStatus)
aicprICPRecordalStatus = lens _aicprICPRecordalStatus (\s a -> s {_aicprICPRecordalStatus = a})

instance FromXML AliasICPRecordal where
  parseXML x =
    AliasICPRecordal'
      <$> (x .@? "CNAME") <*> (x .@? "ICPRecordalStatus")

instance Hashable AliasICPRecordal

instance NFData AliasICPRecordal
