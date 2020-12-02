{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.AppsListDataSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.AppsListDataSummary where

import Network.AWS.FMS.Types.App
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details of the AWS Firewall Manager applications list.
--
--
--
-- /See:/ 'appsListDataSummary' smart constructor.
data AppsListDataSummary = AppsListDataSummary'
  { _aldsListARN ::
      !(Maybe Text),
    _aldsAppsList :: !(Maybe [App]),
    _aldsListId :: !(Maybe Text),
    _aldsListName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AppsListDataSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aldsListARN' - The Amazon Resource Name (ARN) of the applications list.
--
-- * 'aldsAppsList' - An array of @App@ objects in the AWS Firewall Manager applications list.
--
-- * 'aldsListId' - The ID of the applications list.
--
-- * 'aldsListName' - The name of the applications list.
appsListDataSummary ::
  AppsListDataSummary
appsListDataSummary =
  AppsListDataSummary'
    { _aldsListARN = Nothing,
      _aldsAppsList = Nothing,
      _aldsListId = Nothing,
      _aldsListName = Nothing
    }

-- | The Amazon Resource Name (ARN) of the applications list.
aldsListARN :: Lens' AppsListDataSummary (Maybe Text)
aldsListARN = lens _aldsListARN (\s a -> s {_aldsListARN = a})

-- | An array of @App@ objects in the AWS Firewall Manager applications list.
aldsAppsList :: Lens' AppsListDataSummary [App]
aldsAppsList = lens _aldsAppsList (\s a -> s {_aldsAppsList = a}) . _Default . _Coerce

-- | The ID of the applications list.
aldsListId :: Lens' AppsListDataSummary (Maybe Text)
aldsListId = lens _aldsListId (\s a -> s {_aldsListId = a})

-- | The name of the applications list.
aldsListName :: Lens' AppsListDataSummary (Maybe Text)
aldsListName = lens _aldsListName (\s a -> s {_aldsListName = a})

instance FromJSON AppsListDataSummary where
  parseJSON =
    withObject
      "AppsListDataSummary"
      ( \x ->
          AppsListDataSummary'
            <$> (x .:? "ListArn")
            <*> (x .:? "AppsList" .!= mempty)
            <*> (x .:? "ListId")
            <*> (x .:? "ListName")
      )

instance Hashable AppsListDataSummary

instance NFData AppsListDataSummary
