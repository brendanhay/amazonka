{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ReservedInstancesModification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ReservedInstancesModification where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ReservedInstancesId
import Network.AWS.EC2.Types.ReservedInstancesModificationResult
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a Reserved Instance modification.
--
--
--
-- /See:/ 'reservedInstancesModification' smart constructor.
data ReservedInstancesModification = ReservedInstancesModification'
  { _rimModificationResults ::
      !( Maybe
           [ReservedInstancesModificationResult]
       ),
    _rimStatus :: !(Maybe Text),
    _rimClientToken ::
      !(Maybe Text),
    _rimUpdateDate ::
      !(Maybe ISO8601),
    _rimCreateDate ::
      !(Maybe ISO8601),
    _rimEffectiveDate ::
      !(Maybe ISO8601),
    _rimStatusMessage ::
      !(Maybe Text),
    _rimReservedInstancesModificationId ::
      !(Maybe Text),
    _rimReservedInstancesIds ::
      !(Maybe [ReservedInstancesId])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReservedInstancesModification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rimModificationResults' - Contains target configurations along with their corresponding new Reserved Instance IDs.
--
-- * 'rimStatus' - The status of the Reserved Instances modification request.
--
-- * 'rimClientToken' - A unique, case-sensitive key supplied by the client to ensure that the request is idempotent. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- * 'rimUpdateDate' - The time when the modification request was last updated.
--
-- * 'rimCreateDate' - The time when the modification request was created.
--
-- * 'rimEffectiveDate' - The time for the modification to become effective.
--
-- * 'rimStatusMessage' - The reason for the status.
--
-- * 'rimReservedInstancesModificationId' - A unique ID for the Reserved Instance modification.
--
-- * 'rimReservedInstancesIds' - The IDs of one or more Reserved Instances.
reservedInstancesModification ::
  ReservedInstancesModification
reservedInstancesModification =
  ReservedInstancesModification'
    { _rimModificationResults = Nothing,
      _rimStatus = Nothing,
      _rimClientToken = Nothing,
      _rimUpdateDate = Nothing,
      _rimCreateDate = Nothing,
      _rimEffectiveDate = Nothing,
      _rimStatusMessage = Nothing,
      _rimReservedInstancesModificationId = Nothing,
      _rimReservedInstancesIds = Nothing
    }

-- | Contains target configurations along with their corresponding new Reserved Instance IDs.
rimModificationResults :: Lens' ReservedInstancesModification [ReservedInstancesModificationResult]
rimModificationResults = lens _rimModificationResults (\s a -> s {_rimModificationResults = a}) . _Default . _Coerce

-- | The status of the Reserved Instances modification request.
rimStatus :: Lens' ReservedInstancesModification (Maybe Text)
rimStatus = lens _rimStatus (\s a -> s {_rimStatus = a})

-- | A unique, case-sensitive key supplied by the client to ensure that the request is idempotent. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
rimClientToken :: Lens' ReservedInstancesModification (Maybe Text)
rimClientToken = lens _rimClientToken (\s a -> s {_rimClientToken = a})

-- | The time when the modification request was last updated.
rimUpdateDate :: Lens' ReservedInstancesModification (Maybe UTCTime)
rimUpdateDate = lens _rimUpdateDate (\s a -> s {_rimUpdateDate = a}) . mapping _Time

-- | The time when the modification request was created.
rimCreateDate :: Lens' ReservedInstancesModification (Maybe UTCTime)
rimCreateDate = lens _rimCreateDate (\s a -> s {_rimCreateDate = a}) . mapping _Time

-- | The time for the modification to become effective.
rimEffectiveDate :: Lens' ReservedInstancesModification (Maybe UTCTime)
rimEffectiveDate = lens _rimEffectiveDate (\s a -> s {_rimEffectiveDate = a}) . mapping _Time

-- | The reason for the status.
rimStatusMessage :: Lens' ReservedInstancesModification (Maybe Text)
rimStatusMessage = lens _rimStatusMessage (\s a -> s {_rimStatusMessage = a})

-- | A unique ID for the Reserved Instance modification.
rimReservedInstancesModificationId :: Lens' ReservedInstancesModification (Maybe Text)
rimReservedInstancesModificationId = lens _rimReservedInstancesModificationId (\s a -> s {_rimReservedInstancesModificationId = a})

-- | The IDs of one or more Reserved Instances.
rimReservedInstancesIds :: Lens' ReservedInstancesModification [ReservedInstancesId]
rimReservedInstancesIds = lens _rimReservedInstancesIds (\s a -> s {_rimReservedInstancesIds = a}) . _Default . _Coerce

instance FromXML ReservedInstancesModification where
  parseXML x =
    ReservedInstancesModification'
      <$> ( x .@? "modificationResultSet" .!@ mempty
              >>= may (parseXMLList "item")
          )
      <*> (x .@? "status")
      <*> (x .@? "clientToken")
      <*> (x .@? "updateDate")
      <*> (x .@? "createDate")
      <*> (x .@? "effectiveDate")
      <*> (x .@? "statusMessage")
      <*> (x .@? "reservedInstancesModificationId")
      <*> ( x .@? "reservedInstancesSet" .!@ mempty
              >>= may (parseXMLList "item")
          )

instance Hashable ReservedInstancesModification

instance NFData ReservedInstancesModification
