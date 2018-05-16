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
-- Module      : Network.AWS.RDS.ModifyDBClusterSnapshotAttribute
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an attribute and values to, or removes an attribute and values from, a manual DB cluster snapshot.
--
--
-- To share a manual DB cluster snapshot with other AWS accounts, specify @restore@ as the @AttributeName@ and use the @ValuesToAdd@ parameter to add a list of IDs of the AWS accounts that are authorized to restore the manual DB cluster snapshot. Use the value @all@ to make the manual DB cluster snapshot public, which means that it can be copied or restored by all AWS accounts. Do not add the @all@ value for any manual DB cluster snapshots that contain private information that you don't want available to all AWS accounts. If a manual DB cluster snapshot is encrypted, it can be shared, but only by specifying a list of authorized AWS account IDs for the @ValuesToAdd@ parameter. You can't use @all@ as a value for that parameter in this case.
--
-- To view which AWS accounts have access to copy or restore a manual DB cluster snapshot, or whether a manual DB cluster snapshot public or private, use the 'DescribeDBClusterSnapshotAttributes' API action.
--
module Network.AWS.RDS.ModifyDBClusterSnapshotAttribute
    (
    -- * Creating a Request
      modifyDBClusterSnapshotAttribute
    , ModifyDBClusterSnapshotAttribute
    -- * Request Lenses
    , mdcsaValuesToAdd
    , mdcsaValuesToRemove
    , mdcsaDBClusterSnapshotIdentifier
    , mdcsaAttributeName

    -- * Destructuring the Response
    , modifyDBClusterSnapshotAttributeResponse
    , ModifyDBClusterSnapshotAttributeResponse
    -- * Response Lenses
    , mdcsarsDBClusterSnapshotAttributesResult
    , mdcsarsResponseStatus
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
-- /See:/ 'modifyDBClusterSnapshotAttribute' smart constructor.
data ModifyDBClusterSnapshotAttribute = ModifyDBClusterSnapshotAttribute'
  { _mdcsaValuesToAdd                 :: !(Maybe [Text])
  , _mdcsaValuesToRemove              :: !(Maybe [Text])
  , _mdcsaDBClusterSnapshotIdentifier :: !Text
  , _mdcsaAttributeName               :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyDBClusterSnapshotAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mdcsaValuesToAdd' - A list of DB cluster snapshot attributes to add to the attribute specified by @AttributeName@ . To authorize other AWS accounts to copy or restore a manual DB cluster snapshot, set this list to include one or more AWS account IDs, or @all@ to make the manual DB cluster snapshot restorable by any AWS account. Do not add the @all@ value for any manual DB cluster snapshots that contain private information that you don't want available to all AWS accounts.
--
-- * 'mdcsaValuesToRemove' - A list of DB cluster snapshot attributes to remove from the attribute specified by @AttributeName@ . To remove authorization for other AWS accounts to copy or restore a manual DB cluster snapshot, set this list to include one or more AWS account identifiers, or @all@ to remove authorization for any AWS account to copy or restore the DB cluster snapshot. If you specify @all@ , an AWS account whose account ID is explicitly added to the @restore@ attribute can still copy or restore a manual DB cluster snapshot.
--
-- * 'mdcsaDBClusterSnapshotIdentifier' - The identifier for the DB cluster snapshot to modify the attributes for.
--
-- * 'mdcsaAttributeName' - The name of the DB cluster snapshot attribute to modify. To manage authorization for other AWS accounts to copy or restore a manual DB cluster snapshot, set this value to @restore@ .
modifyDBClusterSnapshotAttribute
    :: Text -- ^ 'mdcsaDBClusterSnapshotIdentifier'
    -> Text -- ^ 'mdcsaAttributeName'
    -> ModifyDBClusterSnapshotAttribute
modifyDBClusterSnapshotAttribute pDBClusterSnapshotIdentifier_ pAttributeName_ =
  ModifyDBClusterSnapshotAttribute'
    { _mdcsaValuesToAdd = Nothing
    , _mdcsaValuesToRemove = Nothing
    , _mdcsaDBClusterSnapshotIdentifier = pDBClusterSnapshotIdentifier_
    , _mdcsaAttributeName = pAttributeName_
    }


-- | A list of DB cluster snapshot attributes to add to the attribute specified by @AttributeName@ . To authorize other AWS accounts to copy or restore a manual DB cluster snapshot, set this list to include one or more AWS account IDs, or @all@ to make the manual DB cluster snapshot restorable by any AWS account. Do not add the @all@ value for any manual DB cluster snapshots that contain private information that you don't want available to all AWS accounts.
mdcsaValuesToAdd :: Lens' ModifyDBClusterSnapshotAttribute [Text]
mdcsaValuesToAdd = lens _mdcsaValuesToAdd (\ s a -> s{_mdcsaValuesToAdd = a}) . _Default . _Coerce

-- | A list of DB cluster snapshot attributes to remove from the attribute specified by @AttributeName@ . To remove authorization for other AWS accounts to copy or restore a manual DB cluster snapshot, set this list to include one or more AWS account identifiers, or @all@ to remove authorization for any AWS account to copy or restore the DB cluster snapshot. If you specify @all@ , an AWS account whose account ID is explicitly added to the @restore@ attribute can still copy or restore a manual DB cluster snapshot.
mdcsaValuesToRemove :: Lens' ModifyDBClusterSnapshotAttribute [Text]
mdcsaValuesToRemove = lens _mdcsaValuesToRemove (\ s a -> s{_mdcsaValuesToRemove = a}) . _Default . _Coerce

-- | The identifier for the DB cluster snapshot to modify the attributes for.
mdcsaDBClusterSnapshotIdentifier :: Lens' ModifyDBClusterSnapshotAttribute Text
mdcsaDBClusterSnapshotIdentifier = lens _mdcsaDBClusterSnapshotIdentifier (\ s a -> s{_mdcsaDBClusterSnapshotIdentifier = a})

-- | The name of the DB cluster snapshot attribute to modify. To manage authorization for other AWS accounts to copy or restore a manual DB cluster snapshot, set this value to @restore@ .
mdcsaAttributeName :: Lens' ModifyDBClusterSnapshotAttribute Text
mdcsaAttributeName = lens _mdcsaAttributeName (\ s a -> s{_mdcsaAttributeName = a})

instance AWSRequest ModifyDBClusterSnapshotAttribute
         where
        type Rs ModifyDBClusterSnapshotAttribute =
             ModifyDBClusterSnapshotAttributeResponse
        request = postQuery rds
        response
          = receiveXMLWrapper
              "ModifyDBClusterSnapshotAttributeResult"
              (\ s h x ->
                 ModifyDBClusterSnapshotAttributeResponse' <$>
                   (x .@? "DBClusterSnapshotAttributesResult") <*>
                     (pure (fromEnum s)))

instance Hashable ModifyDBClusterSnapshotAttribute
         where

instance NFData ModifyDBClusterSnapshotAttribute
         where

instance ToHeaders ModifyDBClusterSnapshotAttribute
         where
        toHeaders = const mempty

instance ToPath ModifyDBClusterSnapshotAttribute
         where
        toPath = const "/"

instance ToQuery ModifyDBClusterSnapshotAttribute
         where
        toQuery ModifyDBClusterSnapshotAttribute'{..}
          = mconcat
              ["Action" =:
                 ("ModifyDBClusterSnapshotAttribute" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "ValuesToAdd" =:
                 toQuery
                   (toQueryList "AttributeValue" <$> _mdcsaValuesToAdd),
               "ValuesToRemove" =:
                 toQuery
                   (toQueryList "AttributeValue" <$>
                      _mdcsaValuesToRemove),
               "DBClusterSnapshotIdentifier" =:
                 _mdcsaDBClusterSnapshotIdentifier,
               "AttributeName" =: _mdcsaAttributeName]

-- | /See:/ 'modifyDBClusterSnapshotAttributeResponse' smart constructor.
data ModifyDBClusterSnapshotAttributeResponse = ModifyDBClusterSnapshotAttributeResponse'
  { _mdcsarsDBClusterSnapshotAttributesResult :: !(Maybe DBClusterSnapshotAttributesResult)
  , _mdcsarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyDBClusterSnapshotAttributeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mdcsarsDBClusterSnapshotAttributesResult' - Undocumented member.
--
-- * 'mdcsarsResponseStatus' - -- | The response status code.
modifyDBClusterSnapshotAttributeResponse
    :: Int -- ^ 'mdcsarsResponseStatus'
    -> ModifyDBClusterSnapshotAttributeResponse
modifyDBClusterSnapshotAttributeResponse pResponseStatus_ =
  ModifyDBClusterSnapshotAttributeResponse'
    { _mdcsarsDBClusterSnapshotAttributesResult = Nothing
    , _mdcsarsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
mdcsarsDBClusterSnapshotAttributesResult :: Lens' ModifyDBClusterSnapshotAttributeResponse (Maybe DBClusterSnapshotAttributesResult)
mdcsarsDBClusterSnapshotAttributesResult = lens _mdcsarsDBClusterSnapshotAttributesResult (\ s a -> s{_mdcsarsDBClusterSnapshotAttributesResult = a})

-- | -- | The response status code.
mdcsarsResponseStatus :: Lens' ModifyDBClusterSnapshotAttributeResponse Int
mdcsarsResponseStatus = lens _mdcsarsResponseStatus (\ s a -> s{_mdcsarsResponseStatus = a})

instance NFData
           ModifyDBClusterSnapshotAttributeResponse
         where
