{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.ModifyDBSnapshotAttribute
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an attribute and values to, or removes an attribute and values from, a manual DB snapshot.
--
--
-- To share a manual DB snapshot with other AWS accounts, specify @restore@ as the @AttributeName@ and use the @ValuesToAdd@ parameter to add a list of IDs of the AWS accounts that are authorized to restore the manual DB snapshot. Uses the value @all@ to make the manual DB snapshot public, which means it can be copied or restored by all AWS accounts. Do not add the @all@ value for any manual DB snapshots that contain private information that you don't want available to all AWS accounts. If the manual DB snapshot is encrypted, it can be shared, but only by specifying a list of authorized AWS account IDs for the @ValuesToAdd@ parameter. You can't use @all@ as a value for that parameter in this case.
--
-- To view which AWS accounts have access to copy or restore a manual DB snapshot, or whether a manual DB snapshot public or private, use the 'DescribeDBSnapshotAttributes' API action.
--
module Network.AWS.RDS.ModifyDBSnapshotAttribute
    (
    -- * Creating a Request
      modifyDBSnapshotAttribute
    , ModifyDBSnapshotAttribute
    -- * Request Lenses
    , mdsaValuesToAdd
    , mdsaValuesToRemove
    , mdsaDBSnapshotIdentifier
    , mdsaAttributeName

    -- * Destructuring the Response
    , modifyDBSnapshotAttributeResponse
    , ModifyDBSnapshotAttributeResponse
    -- * Response Lenses
    , mdsarsDBSnapshotAttributesResult
    , mdsarsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.RDS.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'modifyDBSnapshotAttribute' smart constructor.
data ModifyDBSnapshotAttribute = ModifyDBSnapshotAttribute'
  { _mdsaValuesToAdd          :: !(Maybe [Text])
  , _mdsaValuesToRemove       :: !(Maybe [Text])
  , _mdsaDBSnapshotIdentifier :: !Text
  , _mdsaAttributeName        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyDBSnapshotAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mdsaValuesToAdd' - A list of DB snapshot attributes to add to the attribute specified by @AttributeName@ . To authorize other AWS accounts to copy or restore a manual snapshot, set this list to include one or more AWS account IDs, or @all@ to make the manual DB snapshot restorable by any AWS account. Do not add the @all@ value for any manual DB snapshots that contain private information that you don't want available to all AWS accounts.
--
-- * 'mdsaValuesToRemove' - A list of DB snapshot attributes to remove from the attribute specified by @AttributeName@ . To remove authorization for other AWS accounts to copy or restore a manual snapshot, set this list to include one or more AWS account identifiers, or @all@ to remove authorization for any AWS account to copy or restore the DB snapshot. If you specify @all@ , an AWS account whose account ID is explicitly added to the @restore@ attribute can still copy or restore the manual DB snapshot.
--
-- * 'mdsaDBSnapshotIdentifier' - The identifier for the DB snapshot to modify the attributes for.
--
-- * 'mdsaAttributeName' - The name of the DB snapshot attribute to modify. To manage authorization for other AWS accounts to copy or restore a manual DB snapshot, set this value to @restore@ .
modifyDBSnapshotAttribute
    :: Text -- ^ 'mdsaDBSnapshotIdentifier'
    -> Text -- ^ 'mdsaAttributeName'
    -> ModifyDBSnapshotAttribute
modifyDBSnapshotAttribute pDBSnapshotIdentifier_ pAttributeName_ =
  ModifyDBSnapshotAttribute'
    { _mdsaValuesToAdd = Nothing
    , _mdsaValuesToRemove = Nothing
    , _mdsaDBSnapshotIdentifier = pDBSnapshotIdentifier_
    , _mdsaAttributeName = pAttributeName_
    }


-- | A list of DB snapshot attributes to add to the attribute specified by @AttributeName@ . To authorize other AWS accounts to copy or restore a manual snapshot, set this list to include one or more AWS account IDs, or @all@ to make the manual DB snapshot restorable by any AWS account. Do not add the @all@ value for any manual DB snapshots that contain private information that you don't want available to all AWS accounts.
mdsaValuesToAdd :: Lens' ModifyDBSnapshotAttribute [Text]
mdsaValuesToAdd = lens _mdsaValuesToAdd (\ s a -> s{_mdsaValuesToAdd = a}) . _Default . _Coerce

-- | A list of DB snapshot attributes to remove from the attribute specified by @AttributeName@ . To remove authorization for other AWS accounts to copy or restore a manual snapshot, set this list to include one or more AWS account identifiers, or @all@ to remove authorization for any AWS account to copy or restore the DB snapshot. If you specify @all@ , an AWS account whose account ID is explicitly added to the @restore@ attribute can still copy or restore the manual DB snapshot.
mdsaValuesToRemove :: Lens' ModifyDBSnapshotAttribute [Text]
mdsaValuesToRemove = lens _mdsaValuesToRemove (\ s a -> s{_mdsaValuesToRemove = a}) . _Default . _Coerce

-- | The identifier for the DB snapshot to modify the attributes for.
mdsaDBSnapshotIdentifier :: Lens' ModifyDBSnapshotAttribute Text
mdsaDBSnapshotIdentifier = lens _mdsaDBSnapshotIdentifier (\ s a -> s{_mdsaDBSnapshotIdentifier = a})

-- | The name of the DB snapshot attribute to modify. To manage authorization for other AWS accounts to copy or restore a manual DB snapshot, set this value to @restore@ .
mdsaAttributeName :: Lens' ModifyDBSnapshotAttribute Text
mdsaAttributeName = lens _mdsaAttributeName (\ s a -> s{_mdsaAttributeName = a})

instance AWSRequest ModifyDBSnapshotAttribute where
        type Rs ModifyDBSnapshotAttribute =
             ModifyDBSnapshotAttributeResponse
        request = postQuery rds
        response
          = receiveXMLWrapper "ModifyDBSnapshotAttributeResult"
              (\ s h x ->
                 ModifyDBSnapshotAttributeResponse' <$>
                   (x .@? "DBSnapshotAttributesResult") <*>
                     (pure (fromEnum s)))

instance Hashable ModifyDBSnapshotAttribute where

instance NFData ModifyDBSnapshotAttribute where

instance ToHeaders ModifyDBSnapshotAttribute where
        toHeaders = const mempty

instance ToPath ModifyDBSnapshotAttribute where
        toPath = const "/"

instance ToQuery ModifyDBSnapshotAttribute where
        toQuery ModifyDBSnapshotAttribute'{..}
          = mconcat
              ["Action" =:
                 ("ModifyDBSnapshotAttribute" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "ValuesToAdd" =:
                 toQuery
                   (toQueryList "AttributeValue" <$> _mdsaValuesToAdd),
               "ValuesToRemove" =:
                 toQuery
                   (toQueryList "AttributeValue" <$>
                      _mdsaValuesToRemove),
               "DBSnapshotIdentifier" =: _mdsaDBSnapshotIdentifier,
               "AttributeName" =: _mdsaAttributeName]

-- | /See:/ 'modifyDBSnapshotAttributeResponse' smart constructor.
data ModifyDBSnapshotAttributeResponse = ModifyDBSnapshotAttributeResponse'
  { _mdsarsDBSnapshotAttributesResult :: !(Maybe DBSnapshotAttributesResult)
  , _mdsarsResponseStatus             :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyDBSnapshotAttributeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mdsarsDBSnapshotAttributesResult' - Undocumented member.
--
-- * 'mdsarsResponseStatus' - -- | The response status code.
modifyDBSnapshotAttributeResponse
    :: Int -- ^ 'mdsarsResponseStatus'
    -> ModifyDBSnapshotAttributeResponse
modifyDBSnapshotAttributeResponse pResponseStatus_ =
  ModifyDBSnapshotAttributeResponse'
    { _mdsarsDBSnapshotAttributesResult = Nothing
    , _mdsarsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
mdsarsDBSnapshotAttributesResult :: Lens' ModifyDBSnapshotAttributeResponse (Maybe DBSnapshotAttributesResult)
mdsarsDBSnapshotAttributesResult = lens _mdsarsDBSnapshotAttributesResult (\ s a -> s{_mdsarsDBSnapshotAttributesResult = a})

-- | -- | The response status code.
mdsarsResponseStatus :: Lens' ModifyDBSnapshotAttributeResponse Int
mdsarsResponseStatus = lens _mdsarsResponseStatus (\ s a -> s{_mdsarsResponseStatus = a})

instance NFData ModifyDBSnapshotAttributeResponse
         where
