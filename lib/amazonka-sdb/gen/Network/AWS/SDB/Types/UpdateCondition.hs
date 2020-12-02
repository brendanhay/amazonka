{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.Types.UpdateCondition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SDB.Types.UpdateCondition where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the conditions under which data should be updated. If an update condition is specified for a request, the data will only be updated if the condition is satisfied. For example, if an attribute with a specific name and value exists, or if a specific attribute doesn't exist.
--
--
--
-- /See:/ 'updateCondition' smart constructor.
data UpdateCondition = UpdateCondition'
  { _ucExists :: !(Maybe Bool),
    _ucValue :: !(Maybe Text),
    _ucName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateCondition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucExists' - A value specifying whether or not the specified attribute must exist with the specified value in order for the update condition to be satisfied. Specify @true@ if the attribute must exist for the update condition to be satisfied. Specify @false@ if the attribute should not exist in order for the update condition to be satisfied.
--
-- * 'ucValue' - The value of an attribute. This value can only be specified when the @Exists@ parameter is equal to @true@ .
--
-- * 'ucName' - The name of the attribute involved in the condition.
updateCondition ::
  UpdateCondition
updateCondition =
  UpdateCondition'
    { _ucExists = Nothing,
      _ucValue = Nothing,
      _ucName = Nothing
    }

-- | A value specifying whether or not the specified attribute must exist with the specified value in order for the update condition to be satisfied. Specify @true@ if the attribute must exist for the update condition to be satisfied. Specify @false@ if the attribute should not exist in order for the update condition to be satisfied.
ucExists :: Lens' UpdateCondition (Maybe Bool)
ucExists = lens _ucExists (\s a -> s {_ucExists = a})

-- | The value of an attribute. This value can only be specified when the @Exists@ parameter is equal to @true@ .
ucValue :: Lens' UpdateCondition (Maybe Text)
ucValue = lens _ucValue (\s a -> s {_ucValue = a})

-- | The name of the attribute involved in the condition.
ucName :: Lens' UpdateCondition (Maybe Text)
ucName = lens _ucName (\s a -> s {_ucName = a})

instance Hashable UpdateCondition

instance NFData UpdateCondition

instance ToQuery UpdateCondition where
  toQuery UpdateCondition' {..} =
    mconcat
      ["Exists" =: _ucExists, "Value" =: _ucValue, "Name" =: _ucName]
