{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.RoleLastUsed
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.RoleLastUsed where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the last time that an IAM role was used. This includes the date and time and the Region in which the role was last used. Activity is only reported for the trailing 400 days. This period can be shorter if your Region began supporting these features within the last year. The role might have been used more than 400 days ago. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#access-advisor_tracking-period Regions Where Data Is Tracked> in the /IAM User Guide/ .
--
--
-- This data type is returned as a response element in the 'GetRole' and 'GetAccountAuthorizationDetails' operations.
--
--
-- /See:/ 'roleLastUsed' smart constructor.
data RoleLastUsed = RoleLastUsed'
  { _rluLastUsedDate ::
      !(Maybe ISO8601),
    _rluRegion :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RoleLastUsed' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rluLastUsedDate' - The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> that the role was last used. This field is null if the role has not been used within the IAM tracking period. For more information about the tracking period, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#access-advisor_tracking-period Regions Where Data Is Tracked> in the /IAM User Guide/ .
--
-- * 'rluRegion' - The name of the AWS Region in which the role was last used.
roleLastUsed ::
  RoleLastUsed
roleLastUsed =
  RoleLastUsed' {_rluLastUsedDate = Nothing, _rluRegion = Nothing}

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> that the role was last used. This field is null if the role has not been used within the IAM tracking period. For more information about the tracking period, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#access-advisor_tracking-period Regions Where Data Is Tracked> in the /IAM User Guide/ .
rluLastUsedDate :: Lens' RoleLastUsed (Maybe UTCTime)
rluLastUsedDate = lens _rluLastUsedDate (\s a -> s {_rluLastUsedDate = a}) . mapping _Time

-- | The name of the AWS Region in which the role was last used.
rluRegion :: Lens' RoleLastUsed (Maybe Text)
rluRegion = lens _rluRegion (\s a -> s {_rluRegion = a})

instance FromXML RoleLastUsed where
  parseXML x =
    RoleLastUsed' <$> (x .@? "LastUsedDate") <*> (x .@? "Region")

instance Hashable RoleLastUsed

instance NFData RoleLastUsed
