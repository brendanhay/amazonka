{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.ChangeInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.ChangeInfo where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Route53.Internal
import Network.AWS.Route53.Types.ChangeStatus

-- | A complex type that describes change information about changes made to your hosted zone.
--
--
--
-- /See:/ 'changeInfo' smart constructor.
data ChangeInfo = ChangeInfo'
  { _ciComment :: !(Maybe Text),
    _ciId :: !ResourceId,
    _ciStatus :: !ChangeStatus,
    _ciSubmittedAt :: !ISO8601
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ChangeInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ciComment' - A complex type that describes change information about changes made to your hosted zone. This element contains an ID that you use when performing a <https://docs.aws.amazon.com/Route53/latest/APIReference/API_GetChange.html GetChange> action to get detailed information about the change.
--
-- * 'ciId' - The ID of the request.
--
-- * 'ciStatus' - The current state of the request. @PENDING@ indicates that this request has not yet been applied to all Amazon Route 53 DNS servers.
--
-- * 'ciSubmittedAt' - The date and time that the change request was submitted in <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601 format> and Coordinated Universal Time (UTC). For example, the value @2017-03-27T17:48:16.751Z@ represents March 27, 2017 at 17:48:16.751 UTC.
changeInfo ::
  -- | 'ciId'
  ResourceId ->
  -- | 'ciStatus'
  ChangeStatus ->
  -- | 'ciSubmittedAt'
  UTCTime ->
  ChangeInfo
changeInfo pId_ pStatus_ pSubmittedAt_ =
  ChangeInfo'
    { _ciComment = Nothing,
      _ciId = pId_,
      _ciStatus = pStatus_,
      _ciSubmittedAt = _Time # pSubmittedAt_
    }

-- | A complex type that describes change information about changes made to your hosted zone. This element contains an ID that you use when performing a <https://docs.aws.amazon.com/Route53/latest/APIReference/API_GetChange.html GetChange> action to get detailed information about the change.
ciComment :: Lens' ChangeInfo (Maybe Text)
ciComment = lens _ciComment (\s a -> s {_ciComment = a})

-- | The ID of the request.
ciId :: Lens' ChangeInfo ResourceId
ciId = lens _ciId (\s a -> s {_ciId = a})

-- | The current state of the request. @PENDING@ indicates that this request has not yet been applied to all Amazon Route 53 DNS servers.
ciStatus :: Lens' ChangeInfo ChangeStatus
ciStatus = lens _ciStatus (\s a -> s {_ciStatus = a})

-- | The date and time that the change request was submitted in <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601 format> and Coordinated Universal Time (UTC). For example, the value @2017-03-27T17:48:16.751Z@ represents March 27, 2017 at 17:48:16.751 UTC.
ciSubmittedAt :: Lens' ChangeInfo UTCTime
ciSubmittedAt = lens _ciSubmittedAt (\s a -> s {_ciSubmittedAt = a}) . _Time

instance FromXML ChangeInfo where
  parseXML x =
    ChangeInfo'
      <$> (x .@? "Comment")
      <*> (x .@ "Id")
      <*> (x .@ "Status")
      <*> (x .@ "SubmittedAt")

instance Hashable ChangeInfo

instance NFData ChangeInfo
